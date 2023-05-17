use crate::matcher::{RegexMatcher, TextMatcher};
use crate::render::{self, Render};
use crate::source::{GrepOptions, GrepResult, Node, Source};
use anyhow::Result;
use clap::Parser;
use clap_stdin::MaybeStdIn;
use colored::*;
use ignore::{overrides::OverrideBuilder, types::TypesBuilder, WalkBuilder};
use rayon::prelude::*;
use std::io::StdoutLock;
use std::{
    env, fs,
    io::{stdout, BufWriter, Write},
    path::Path,
    vec,
};
use tap::Tap;

#[derive(Parser)]
#[command(name = "rbgrep")]
#[command(author = "Takahiro Sato. <harehare1110@gmail.com>")]
#[command(version = "0.1.3")]
#[command(
    about = "rbgrep is a line-oriented search cli tool that recursively searches ruby files in the current directory for a regex patterns.",
    long_about = None
)]
pub struct Cli {
    /// Only print the count of individual matches for each file.
    #[arg(short, long)]
    count: bool,

    /// Case insensitive search.
    #[arg(short, long)]
    ignore_case: bool,

    /// Exact match Search.
    #[arg(short = 'm', long)]
    exact_match: bool,

    /// Search hidden files and directory.
    #[arg(short = '.', long)]
    hidden: bool,

    /// Regex pattern search.
    #[arg(short = 'e', long)]
    regexp: bool,

    /// Start nodes.
    #[arg(short = 'S', long, value_enum)]
    start_nodes: Option<Vec<Node>>,

    /// If specified, it excludes files or directories matching the given filename pattern from the search.
    #[arg(long)]
    exclude: Option<String>,

    /// End nodes.
    #[arg(short = 'E', long, value_enum)]
    end_nodes: Option<Vec<Node>>,

    /// Don't respect .gitignore files.
    #[arg(long)]
    no_git_ignore: bool,

    /// Never print the file path with the matched lines.
    #[arg(short = 'N', long)]
    no_file_name: bool,

    /// Never print the line number with the matched lines.
    #[arg(long)]
    no_line_no: bool,

    /// Not colored the output results.
    #[arg(long)]
    no_color: bool,

    /// Print only matched parts of a line.
    #[arg(short, long)]
    only_matching: bool,

    /// Show lines before and after each match.
    #[arg(short = 'C', long)]
    context: Option<usize>,

    /// Show lines before each match.
    #[arg(short = 'A', long)]
    after_context: Option<usize>,

    /// Show lines before each match.
    #[arg(short = 'B', long)]
    before_context: Option<usize>,

    /// The string used to separate.
    #[arg(long)]
    context_separator: Option<String>,

    /// Print nodes with the matched lines.
    #[arg(long)]
    with_nodes: bool,

    /// Print warning after parse of ruby file.
    #[arg(long)]
    with_warning: bool,

    /// Number of grep worker threads to use.
    #[arg(long)]
    threads: Option<usize>,

    /// The maximum depth to recurse.
    #[arg(long)]
    max_depth: Option<usize>,

    /// Do not output matched lines. instead, exit with status 0 when there is a match and with non-zero status when there isn’t.
    #[arg(short, long)]
    quiet: bool,

    /// Searches for specified files and directories
    #[arg(short, long)]
    path: Option<Vec<String>>,

    query: String,
    stdin: Option<MaybeStdIn<String>>,
}

const DEFAULT_SEPARATOR: &str = "--";

impl Cli {
    pub fn run(&self) -> Result<()> {
        if self.no_color {
            env::set_var("NO_COLOR", "true")
        }

        if self.threads.is_some() {
            env::set_var(
                "RAYON_NUM_THREADS",
                self.threads.map(|n| n.to_string()).unwrap(),
            );
        }

        let path_list = match &self.path {
            Some(p) => p.clone(),
            None => vec![".".to_string()],
        };

        if self.regexp {
            match RegexMatcher::new(self.query.as_str()) {
                Ok(m) => {
                    let results = match self.stdin.as_ref() {
                        Some(stdin) => {
                            let source = Source::new(
                                stdin.as_str(),
                                &m,
                                GrepOptions {
                                    start_nodes: self.start_nodes.clone(),
                                    end_nodes: self.end_nodes.clone(),
                                },
                            );

                            source
                                .grep("")
                                .map(|r| {
                                    self.print(&r);
                                    vec![r]
                                })
                                .unwrap_or(vec![])
                        }
                        None => path_list
                            .par_iter()
                            .flat_map(|path| {
                                self.entries(path)
                                    .par_iter()
                                    .filter_map(|path| {
                                        fs::read_to_string(path)
                                            .ok()
                                            .and_then(|content| {
                                                let source = Source::new(
                                                    content.as_str(),
                                                    &m,
                                                    GrepOptions {
                                                        start_nodes: self.start_nodes.clone(),
                                                        end_nodes: self.end_nodes.clone(),
                                                    },
                                                );

                                                source
                                                    .errors(path.clone(), self.with_warning)
                                                    .or(source.grep(path))
                                            })
                                            .map(|ret| ret.tap(|r| self.print(r)))
                                    })
                                    .collect::<Vec<GrepResult>>()
                            })
                            .collect::<Vec<GrepResult>>(),
                    };

                    if results.is_empty() {
                        Err(anyhow::anyhow!("not found"))
                    } else {
                        Ok(())
                    }
                }
                Err(e) => Err(anyhow::anyhow!(e)),
            }
        } else {
            let m = TextMatcher::new(self.query.to_string(), self.exact_match, !self.ignore_case);
            let results = match self.stdin.as_ref() {
                Some(stdin) => {
                    let source = Source::new(
                        stdin.as_str(),
                        &m,
                        GrepOptions {
                            start_nodes: self.start_nodes.clone(),
                            end_nodes: self.end_nodes.clone(),
                        },
                    );

                    source
                        .grep("")
                        .map(|r| {
                            self.print(&r);
                            vec![r]
                        })
                        .unwrap_or(vec![])
                }
                None => path_list
                    .par_iter()
                    .flat_map(|path| {
                        self.entries(path)
                            .par_iter()
                            .filter_map(|path| {
                                fs::read_to_string(path)
                                    .ok()
                                    .and_then(|content| {
                                        let source = Source::new(
                                            content.as_str(),
                                            &m,
                                            GrepOptions {
                                                start_nodes: self.start_nodes.clone(),
                                                end_nodes: self.end_nodes.clone(),
                                            },
                                        );

                                        source
                                            .errors(path.clone(), self.with_warning)
                                            .or(source.grep(path))
                                    })
                                    .map(|ret| ret.tap(|r| self.print(r)))
                            })
                            .collect::<Vec<GrepResult>>()
                    })
                    .collect::<Vec<GrepResult>>(),
            };

            if results.is_empty() {
                Err(anyhow::anyhow!("not found"))
            } else {
                Ok(())
            }
        }
    }

    fn print(&self, result: &GrepResult) {
        let mut out = BufWriter::new(stdout().lock());

        match result {
            GrepResult::FileResult(r) => {
                if self.count {
                    let count_render = render::CountRender {
                        with_filename: !self.no_file_name,
                    };
                    count_render
                        .render::<BufWriter<StdoutLock>>(&mut out, r)
                        .expect("write failed");
                } else if !self.quiet {
                    let text_render = render::TextRender {
                        with_nodes: self.with_nodes,
                        with_filename: !self.no_file_name,
                        with_lineno: !self.no_line_no,
                        only_matching: self.only_matching,
                        separator: self
                            .context_separator
                            .clone()
                            .unwrap_or(DEFAULT_SEPARATOR.to_string()),
                        before_context: self.context.or(self.before_context),
                        after_context: self.context.or(self.after_context),
                    };
                    text_render
                        .render::<BufWriter<StdoutLock>>(&mut out, r)
                        .expect("write failed");
                }
            }
            GrepResult::FileErrorResult(errors) => {
                out.write_all(format!("{}\n", errors).as_bytes())
                    .expect("write failed");
            }
        }
    }

    fn entries(&self, path: &str) -> Vec<String> {
        if Path::new(path).is_file() {
            vec![path.to_string()]
        } else {
            let mut walk_builder = WalkBuilder::new(path);
            let mut types_builder = TypesBuilder::new();
            types_builder.add_defaults();
            types_builder.add("ruby", "*.rb").unwrap();
            types_builder.select("ruby");

            let mut overrides_builder = OverrideBuilder::new(".");

            if let Some(exclude) = &self.exclude {
                overrides_builder
                    .add(format!("!{}", exclude).as_str())
                    .unwrap();
                walk_builder.overrides(overrides_builder.build().unwrap());
            }

            let file_matcher = types_builder.build().unwrap();

            walk_builder
                .git_ignore(!self.no_git_ignore)
                .git_exclude(!self.no_git_ignore)
                .hidden(self.hidden)
                .max_depth(self.max_depth)
                .types(file_matcher)
                .build()
                .filter_map(|entry| {
                    entry
                        .map_err(|err| println!("{}", err.to_string().bold().red()))
                        .ok()
                })
                .map(|entry| entry.path().to_str().unwrap().to_string())
                .collect::<Vec<String>>()
        }
    }
}

#[cfg(test)]
mod tests {
    use clap_stdin::MaybeStdIn;
    use rstest::rstest;
    use std::io::prelude::*;
    use std::str::FromStr;
    use std::{env, fs::File};
    use tempfile::NamedTempFile;

    #[rstest]
    #[case(
        "test",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        false,
        false,
        true
    )]
    #[case(
        "no_match",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        false,
        false,
        false
    )]
    #[case(
        "te.+",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        true,
        false,
        true
    )]
    #[case(
        "no_match.+",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        false,
        false,
        false
    )]
    #[case(
        "test",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        false,
        true,
        true
    )]
    fn test_run_with_directory(
        #[case] query: String,
        #[case] text: String,
        #[case] regexp: bool,
        #[case] quiet: bool,
        #[case] expected: bool,
    ) {
        let dir = env::temp_dir();
        let file = dir.join("test.rb");
        let mut tmp_file = File::create(file).unwrap();
        tmp_file.write_all(text.as_bytes()).unwrap();

        let cli = crate::Cli {
            count: false,
            ignore_case: true,
            exact_match: false,
            hidden: true,
            regexp,
            start_nodes: None,
            end_nodes: None,
            only_matching: false,
            no_git_ignore: false,
            no_file_name: false,
            no_line_no: false,
            no_color: true,
            context: None,
            after_context: None,
            before_context: None,
            context_separator: None,
            with_nodes: false,
            with_warning: false,
            threads: Some(1),
            max_depth: None,
            quiet,
            exclude: Some("test".to_string()),
            query: query,
            path: Some(vec![dir.to_str().unwrap().to_string()]),
            stdin: None,
        };

        assert_eq!(cli.run().is_ok(), expected);
    }

    #[rstest]
    #[case(
        "test",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        false,
        true
    )]
    #[case(
        "test",
        "class Test\n  def test\n    puts 'test'\n  end\nend",
        true,
        true
    )]
    fn test_run_with_file(
        #[case] query: String,
        #[case] text: String,
        #[case] is_stdin: bool,
        #[case] expected: bool,
    ) {
        let mut file = NamedTempFile::new().unwrap();
        file.write_all(text.as_bytes()).unwrap();

        let cli = crate::Cli {
            count: false,
            ignore_case: true,
            exact_match: false,
            hidden: true,
            regexp: false,
            start_nodes: None,
            end_nodes: None,
            only_matching: false,
            no_git_ignore: false,
            no_file_name: false,
            no_line_no: false,
            no_color: true,
            context: None,
            after_context: None,
            before_context: None,
            context_separator: None,
            with_nodes: false,
            with_warning: false,
            threads: Some(1),
            max_depth: None,
            quiet: false,
            exclude: Some("test".to_string()),
            query: query,
            path: Some(vec![file.path().to_str().unwrap().to_string()]),
            stdin: is_stdin.then_some(MaybeStdIn::from_str(text.as_str()).unwrap()),
        };

        assert_eq!(cli.run().is_ok(), expected);
    }
}
