use std::io;

use anyhow::{anyhow, Result};
use colored::*;

use crate::render::Render;
use crate::source::FileResult;

pub struct TextRender {
    pub with_nodes: bool,
    pub with_filename: bool,
    pub with_lineno: bool,
    pub only_matching: bool,
    pub separator: String,
    pub before_context: Option<usize>,
    pub after_context: Option<usize>,
}

impl Render for TextRender {
    fn render<W: io::Write>(&self, w: &mut W, result: &FileResult) -> Result<()> {
        let result_lines = result
            .results
            .iter()
            .map(|r| {
                let before = self.before_context.map(|b| {
                    let start = if r.row < b { 0 } else { r.row - b };
                    let before_context_lines = &result.lines[start..r.row];

                    if self.with_filename {
                        before_context_lines
                            .iter()
                            .enumerate()
                            .map(|(i, l)| {
                                format!(
                                    "{}{} {}\n",
                                    result.filename.magenta(),
                                    self.with_lineno
                                        .then(|| format!(
                                            "{}{}",
                                            ":".cyan(),
                                            if r.row > (b - i) {
                                                r.row - (b - i) + 1
                                            } else {
                                                1
                                            }
                                        ))
                                        .unwrap_or("".to_string()),
                                    l
                                )
                            })
                            .collect::<Vec<String>>()
                            .join("")
                    } else {
                        before_context_lines
                            .iter()
                            .map(|l| format!("{}\n", l))
                            .collect::<Vec<String>>()
                            .join("")
                    }
                });

                let nodes = if self.with_nodes {
                    Some(format!("{}\n", r.to_nodes_string().on_blue()))
                } else {
                    None
                };

                let result_line = if self.with_filename {
                    Some(format!(
                        "{}{} {}\n",
                        result.filename.magenta(),
                        self.with_lineno
                            .then(|| format!("{}{}", ":".cyan(), (r.row + 1).to_string().cyan()))
                            .unwrap_or("".to_string()),
                        r.to_result_string(self.only_matching),
                    ))
                } else {
                    Some(format!("{}\n", r.to_result_string(self.only_matching)))
                };

                let after = self.after_context.map(|a| {
                    let after_context_lines = if r.row + a < result.lines.len() - 1 {
                        &result.lines[(r.row + 1)..(r.row + a + 1)]
                    } else {
                        &result.lines[(r.row + 1)..]
                    };

                    if self.with_filename {
                        after_context_lines
                            .iter()
                            .enumerate()
                            .map(|(i, l)| {
                                format!(
                                    "{}{} {}\n",
                                    result.filename.magenta(),
                                    self.with_lineno
                                        .then(|| format!(
                                            "{}{}",
                                            ":".cyan(),
                                            if r.row + i + 1 > result.lines.len() - 1 {
                                                result.lines.len() - 1
                                            } else {
                                                r.row + i + 1
                                            }
                                        ))
                                        .unwrap_or("".to_string()),
                                    l
                                )
                            })
                            .collect::<Vec<String>>()
                            .join("")
                    } else {
                        after_context_lines
                            .iter()
                            .map(|l| format!("{}\n", l))
                            .collect::<Vec<String>>()
                            .join("")
                    }
                });

                let separator = if self.before_context.is_some()
                    || self.after_context.is_some()
                    || self.with_nodes
                {
                    Some(format!("{}\n", self.separator.white()))
                } else {
                    None
                };

                vec![before, nodes, result_line, after, separator]
                    .into_iter()
                    .flatten()
                    .collect::<Vec<String>>()
                    .join("")
            })
            .collect::<Vec<String>>()
            .join("");

        w.write_all(result_lines.as_bytes())
            .map_err(|_| anyhow!("Failed write"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        node::{Node, Nodes},
        source::LineResult,
    };
    use rstest::rstest;

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        false,
        false,
        None,
        None,
        "class Test\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        true,
        false,
        None,
        None,
        "file class Test\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        true,
        true,
        None,
        None,
        "file:1 class Test\n".to_string(),
    )]
    // before_context
    #[case(
        vec!["class Test"," def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![LineResult {line: "def test".to_string(), row: 1, column_start: 5, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        true,
        true,
        Some(1),
        None,
        "file:1 class Test\nfile:2 def test\n--\n".to_string(),
    )]
    #[case(
        vec!["class Test"," def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![LineResult {line: "def test".to_string(), row: 1, column_start: 5, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        false,
        false,
        Some(1),
        None,
        "class Test\ndef test\n--\n".to_string(),
    )]
    // after_context
    #[case(
        vec!["class Test", "def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![LineResult {line: "def test".to_string(), row: 1, column_start: 5, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        true,
        true,
        None,
        Some(1),
        "file:2 def test\nfile:2 end\n--\n".to_string(),
    )]
    #[case(
        vec!["class Test", "def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![LineResult {line: "def test".to_string(), row: 1, column_start: 5, column_end: 8, nodes: Nodes::new(vec![Node::Class])}],
        false,
        false,
        false,
        None,
        Some(1),
        "def test\nend\n--\n".to_string(),
    )]
    //with_nodes
    #[case(
        vec!["class Test", "def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![LineResult {line: "def test".to_string(), row: 1, column_start: 5, column_end: 8, nodes: Nodes::new(vec![Node::Class, Node::Def])}],
        true,
        false,
        false,
        None,
        None,
        "Class > Def\ndef test\n--\n".to_string(),
    )]
    #[case(
        vec!["class Test", "def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![LineResult {line: "def test".to_string(), row: 1, column_start: 5, column_end: 8, nodes: Nodes::new(vec![Node::Class, Node::Def])}],
        true,
        true,
        true,
        None,
        None,
        "Class > Def\nfile:2 def test\n--\n".to_string(),
    )]
    fn test_render_result(
        #[case] lines: Vec<String>,
        #[case] results: Vec<LineResult>,
        #[case] with_nodes: bool,
        #[case] with_filename: bool,
        #[case] with_lineno: bool,
        #[case] before_context: Option<usize>,
        #[case] after_context: Option<usize>,
        #[case] expected: String,
    ) {
        let text_render = TextRender {
            with_nodes,
            with_filename,
            with_lineno,
            only_matching: false,
            separator: "--".to_string(),
            before_context,
            after_context,
        };

        let result = FileResult {
            filename: "file".to_string(),
            lines,
            results,
        };
        let mut out: Vec<u8> = vec![];
        text_render.render::<Vec<u8>>(&mut out, &result).unwrap();

        assert_eq!(String::from_utf8(out).unwrap(), expected);
    }
}
