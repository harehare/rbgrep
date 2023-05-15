use crate::matcher::Matcher;
use clap::ValueEnum;
use colored::*;
use itertools::Itertools;
use lib_ruby_parser::{source::DecodedInput, Diagnostic, Loc, Parser, ParserOptions, ParserResult};
use std::{fmt, vec};

#[derive(Debug, PartialEq, Clone, ValueEnum, strum_macros::Display)]
pub enum Node {
    Alias,
    And,
    AndAsgn,
    Arg,
    Args,
    Array,
    ArrayPattern,
    ArrayPatternWithTail,
    BackRef,
    Begin,
    Block,
    BlockPass,
    Blockarg,
    Break,
    CSend,
    Case,
    CaseMatch,
    Casgn,
    Cbase,
    Class,
    Complex,
    Const,
    ConstPattern,
    Cvar,
    Cvasgn,
    Def,
    Defined,
    Defs,
    Dstr,
    Dsym,
    EFlipFlop,
    EmptyElse,
    Encoding,
    Ensure,
    Erange,
    False,
    File,
    FindPattern,
    Float,
    For,
    ForwardArg,
    ForwardedArgs,
    Gvar,
    Gvasgn,
    Hash,
    HashPattern,
    Heredoc,
    IFlipFlop,
    If,
    IfGuard,
    IfMod,
    IfTernary,
    InPattern,
    Index,
    IndexAsgn,
    Int,
    Irange,
    Ivar,
    Ivasgn,
    KwBegin,
    Kwarg,
    Kwargs,
    Kwnilarg,
    Kwoptarg,
    Kwrestarg,
    Kwsplat,
    Lambda,
    Line,
    Lvar,
    Lvasgn,
    Masgn,
    MatchAlt,
    MatchAs,
    MatchCurrentLine,
    MatchNilPattern,
    MatchPattern,
    MatchPatternP,
    MatchRest,
    MatchVar,
    MatchWithLvasgn,
    Mlhs,
    Module,
    Next,
    Nil,
    NthRef,
    Numblock,
    OpAsgn,
    Optarg,
    Or,
    OrAsgn,
    Pair,
    Pin,
    Postexe,
    Preexe,
    Procarg0,
    Rational,
    Redo,
    RegOpt,
    Regexp,
    Rescue,
    RescueBody,
    Restarg,
    Retry,
    Return,
    SClass,
    Self_,
    Send,
    Shadowarg,
    Splat,
    Str,
    Super,
    Sym,
    True,
    Undef,
    UnlessGuard,
    Until,
    UntilPost,
    When,
    While,
    WhilePost,
    XHeredoc,
    Xstr,
    Yield,
    ZSuper,
}

pub struct GrepOptions {
    pub start_nodes: Option<Vec<Node>>,
    pub end_nodes: Option<Vec<Node>>,
}

pub enum GrepResult {
    FileResult(FileResult),
    FileErrorResult(FileErrorResult),
}

#[derive(Debug)]
pub struct LineErrorResult {
    message: String,
    is_warning: bool,
    loc: (usize, usize),
}

impl fmt::Display for LineErrorResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_warning {
            write!(
                f,
                "{}{} {}",
                (self.loc.0 + 1).to_string().green(),
                ":".cyan(),
                self.message.yellow().bold()
            )
        } else {
            write!(
                f,
                "{}{} {}",
                (self.loc.0 + 1).to_string().green(),
                ":".cyan(),
                self.message.red().bold()
            )
        }
    }
}

#[derive(Debug)]
pub struct FileErrorResult {
    path: String,
    errors: Vec<LineErrorResult>,
}

impl fmt::Display for FileErrorResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            self.errors
                .iter()
                .map(|e| format!("{}{} {}", self.path.magenta(), ":".cyan(), e))
                .join("\n")
        )
    }
}

#[derive(Debug)]
pub struct FileResult {
    filename: String,
    lines: Vec<String>,
    results: Vec<LineResult>,
}

impl FileResult {
    pub fn to_result_string(
        &self,
        with_nodes: bool,
        with_filename: bool,
        with_lineno: bool,
        separator: String,
        before_context: Option<usize>,
        after_context: Option<usize>,
    ) -> String {
        self.results
            .iter()
            .map(|r| {
                let before = before_context.map(|b| {
                    if with_filename {
                        self.before_context(r.row, b)
                            .iter()
                            .enumerate()
                            .map(|(i, l)| {
                                format!(
                                    "{}{} {}\n",
                                    self.filename.magenta(),
                                    with_lineno
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
                        self.before_context(r.row, b)
                            .iter()
                            .map(|l| format!("{}\n", l))
                            .join("")
                    }
                });

                let nodes = if with_nodes {
                    Some(format!("{}\n", r.to_nodes_string().on_blue()))
                } else {
                    None
                };

                let result = if with_filename {
                    Some(format!(
                        "{}{} {}\n",
                        self.filename.magenta(),
                        with_lineno
                            .then(|| format!("{}{}", ":".cyan(), (r.row + 1).to_string().cyan()))
                            .unwrap_or("".to_string()),
                        r,
                    ))
                } else {
                    Some(format!("{}\n", r))
                };

                let after = after_context.map(|a| {
                    if with_filename {
                        self.after_context(r.row, a)
                            .iter()
                            .enumerate()
                            .map(|(i, l)| {
                                format!(
                                    "{}{} {}\n",
                                    self.filename.magenta(),
                                    with_lineno
                                        .then(|| format!(
                                            "{}{}",
                                            ":".cyan(),
                                            if r.row + i + 1 > self.lines.len() - 1 {
                                                self.lines.len() - 1
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
                        self.after_context(r.row, a)
                            .iter()
                            .map(|l| format!("{}\n", l))
                            .join("")
                    }
                });

                let separator = if before_context.is_some() || after_context.is_some() || with_nodes
                {
                    Some(format!("{}\n", separator.white()))
                } else {
                    None
                };

                vec![before, nodes, result, after, separator]
                    .into_iter()
                    .flatten()
                    .join("")
            })
            .join("")
    }

    pub fn to_count_string(&self, with_filename: bool) -> String {
        if with_filename {
            format!("{}:{}\n", self.filename.magenta(), &self.results.len(),)
        } else {
            format!("{}\n", &self.results.len())
        }
    }

    fn before_context(&self, row: usize, num: usize) -> &[String] {
        let start = if row < num { 0 } else { row - num };
        &self.lines[start..row]
    }

    fn after_context(&self, row: usize, num: usize) -> &[String] {
        if row + num < self.lines.len() - 1 {
            &self.lines[(row + 1)..(row + num + 1)]
        } else {
            &self.lines[(row + 1)..]
        }
    }
}

#[derive(Debug)]
pub struct LineResult {
    line: String,
    row: usize,
    nodes: Vec<Node>,
    column_start: usize,
    column_end: usize,
}

impl LineResult {
    pub fn new(
        row: usize,
        line: String,
        nodes: Vec<Node>,
        column_start: usize,
        column_end: usize,
    ) -> Self {
        LineResult {
            row,
            line,
            nodes,
            column_start,
            column_end,
        }
    }

    pub fn to_nodes_string(&self) -> String {
        self.nodes
            .clone()
            .iter()
            .map(|node| node.to_string())
            .collect::<Vec<String>>()
            .join(" > ")
    }
}

impl fmt::Display for LineResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let start_text = &self.line[..self.column_start];
        let match_text = &self.line[self.column_start..self.column_end];
        let end = if match_text.starts_with('"') || match_text.starts_with('\'') {
            self.column_end + 2
        } else {
            self.column_end
        };
        let match_text = &self.line[self.column_start..end];
        let end_text = &self.line[end..];

        write!(f, "{}{}{}", start_text, match_text.red().bold(), end_text,)
    }
}

pub struct Source<'a, T: Matcher> {
    lines: Vec<String>,
    root: Option<Box<lib_ruby_parser::Node>>,
    input: DecodedInput,
    options: GrepOptions,
    diagnostics: Vec<Diagnostic>,
    matcher: &'a T,
}

impl<'a, T: Matcher> Source<'a, T> {
    pub fn new(code: &str, matcher: &'a T, options: GrepOptions) -> Source<'a, T> {
        let parser = Parser::new(
            code.as_bytes().to_vec(),
            ParserOptions {
                buffer_name: "(eval)".to_string(),
                ..Default::default()
            },
        );
        let ParserResult {
            ast,
            input,
            diagnostics,
            ..
        } = parser.do_parse();

        Source {
            lines: code.split('\n').map(|l| l.to_owned()).collect(),
            root: ast,
            input,
            options,
            diagnostics,
            matcher,
        }
    }

    fn error_messages(&self) -> Vec<((usize, usize), bool, String)> {
        self.diagnostics
            .iter()
            .map(|d| {
                let loc = self.input.line_col_for_pos(d.loc.begin).unwrap_or((0, 0));

                if d.is_warning() {
                    (loc, true, format!("{}", d.render_message().yellow()))
                } else {
                    (loc, false, format!("{}", d.render_message().red()))
                }
            })
            .collect::<Vec<((usize, usize), bool, String)>>()
    }

    pub fn errors(&self, path: String, is_print_warning: bool) -> Option<GrepResult> {
        if self.diagnostics.is_empty() {
            None
        } else {
            let errors = self
                .error_messages()
                .iter()
                .filter_map(|(loc, is_warning, message)| {
                    if !is_print_warning && *is_warning {
                        None
                    } else {
                        Some(LineErrorResult {
                            message: message.clone(),
                            is_warning: *is_warning,
                            loc: *loc,
                        })
                    }
                })
                .collect::<Vec<LineErrorResult>>();

            if errors.is_empty() {
                None
            } else {
                Some(GrepResult::FileErrorResult(FileErrorResult {
                    path,
                    errors,
                }))
            }
        }
    }

    pub fn grep(&self, filename: &str) -> Option<GrepResult> {
        self.root.as_ref().and_then(|root| {
            let results: Vec<LineResult> = self
                .search(vec![], root, &self.input)
                .into_iter()
                .filter(|r| self.start_nodes(&r.nodes) && self.end_nodes(&r.nodes))
                .collect();

            (!results.is_empty())
                .then(|| {
                    Some(GrepResult::FileResult(FileResult {
                        filename: filename.to_string(),
                        lines: self.lines.clone(),
                        results,
                    }))
                })
                .unwrap_or(None)
        })
    }

    fn start_nodes(&self, nodes: &[Node]) -> bool {
        self.options
            .start_nodes
            .as_ref()
            .map(|s| nodes.starts_with(s))
            .unwrap_or(true)
    }

    fn end_nodes(&self, nodes: &[Node]) -> bool {
        self.options
            .end_nodes
            .as_ref()
            .map(|e| nodes.ends_with(e))
            .unwrap_or(true)
    }

    fn search(
        &self,
        parent: Vec<Node>,
        node: &lib_ruby_parser::Node,
        input: &DecodedInput,
    ) -> Vec<LineResult> {
        match node {
            lib_ruby_parser::Node::Alias(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Alias]]),
                    &node.to,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::Alias]]),
                    &node.from,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::And(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::And]]),
                    &node.lhs,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::And]]),
                    &node.rhs,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::AndAsgn(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::AndAsgn]]),
                    &node.recv,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::AndAsgn]]),
                    &node.value,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Arg(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Arg]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Args(node) => node
                .args
                .iter()
                .flat_map(|arg| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Arg]]),
                        arg,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Array(node) => node
                .elements
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Array]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::ArrayPattern(node) => node
                .elements
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::ArrayPattern]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::ArrayPatternWithTail(node) => node
                .elements
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::ArrayPatternWithTail]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::BackRef(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::BackRef]]),
                    &node.name,
                    node.expression_l,
                    input,
                    0,
                )
                .map(|r| vec![r])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Begin(node) => node
                .statements
                .iter()
                .flat_map(|node| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Begin]]),
                        node,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Block(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Block]]),
                    &node.call,
                    input,
                ),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Block]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.args
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Block]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::BlockPass(node) => node
                .value
                .as_ref()
                .map(|v| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::BlockPass]]),
                        v,
                        input,
                    )
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Blockarg(node) => self
                .matcher
                .is_match(&node.name.clone().unwrap_or("".to_string()))
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Blockarg]]),
                                pos.1,
                                pos.1 + node.name.clone().unwrap_or("".to_string()).len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Break(node) => node
                .args
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Break]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::CSend(node) => self
                .search_node(
                    itertools::concat(vec![parent.clone(), vec![Node::CSend]]),
                    &node.method_name,
                    node.selector_l.unwrap_or(node.expression_l),
                    input,
                    0,
                )
                .map(|r| {
                    itertools::concat(vec![
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::CSend]]),
                            &node.recv,
                            input,
                        ),
                        vec![r],
                        node.args
                            .iter()
                            .flat_map(|statement| {
                                self.search(
                                    itertools::concat(vec![parent.clone(), vec![Node::CSend]]),
                                    statement,
                                    input,
                                )
                            })
                            .collect(),
                    ])
                })
                .unwrap_or(itertools::concat(vec![
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::CSend]]),
                        &node.recv,
                        input,
                    ),
                    node.args
                        .iter()
                        .flat_map(|statement| {
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::CSend]]),
                                statement,
                                input,
                            )
                        })
                        .collect(),
                ])),

            lib_ruby_parser::Node::Case(node) => itertools::concat(vec![
                node.expr
                    .as_ref()
                    .map(|expr| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Case]]),
                            expr,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.else_body
                    .as_ref()
                    .map(|else_body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Case]]),
                            else_body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.when_bodies
                    .iter()
                    .flat_map(|arg| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Case]]),
                            arg,
                            input,
                        )
                    })
                    .collect(),
            ]),

            lib_ruby_parser::Node::CaseMatch(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::CaseMatch]]),
                    &node.expr,
                    input,
                ),
                node.else_body
                    .as_ref()
                    .map(|else_body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::CaseMatch]]),
                            else_body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.in_bodies
                    .iter()
                    .flat_map(|arg| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::CaseMatch]]),
                            arg,
                            input,
                        )
                    })
                    .collect(),
            ]),

            lib_ruby_parser::Node::Casgn(node) => itertools::concat(vec![
                node.scope
                    .as_ref()
                    .map(|scope| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Casgn]]),
                            scope,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.value
                    .as_ref()
                    .map(|value| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Casgn]]),
                            value,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search_node(
                    itertools::concat(vec![parent, vec![Node::Casgn]]),
                    &node.name,
                    node.name_l,
                    input,
                    0,
                )
                .map(|x| vec![x])
                .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Cbase(node) => self
                .matcher
                .is_match("::")
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::Cbase]]),
                            pos.1,
                            pos.1 + "::".len(),
                        )
                    }) {
                        Some(result) => vec![result],
                        None => vec![],
                    }
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Complex(node) => {
                match self.search_node(
                    itertools::concat(vec![parent, vec![Node::Complex]]),
                    &node.value,
                    node.expression_l,
                    input,
                    0,
                ) {
                    Some(result) => vec![result],
                    None => vec![],
                }
            }

            lib_ruby_parser::Node::Class(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Class]]),
                    &node.name,
                    input,
                ),
                node.superclass
                    .as_ref()
                    .map(|superclass| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Class]]),
                            superclass,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Class]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Const(node) => itertools::concat(vec![
                self.search_node(parent.clone(), &node.name, node.name_l, input, 0)
                    .map(|node| vec![node])
                    .unwrap_or(vec![]),
                node.scope
                    .as_ref()
                    .map(|scope| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Const]]),
                            scope,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::ConstPattern(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::ConstPattern]]),
                    &node.const_,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::ConstPattern]]),
                    &node.pattern,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Cvar(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::Cvar]]),
                    &node.name,
                    node.expression_l,
                    input,
                    0,
                )
                .map(|r| vec![r])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Cvasgn(node) => itertools::concat(vec![
                self.search_node(
                    itertools::concat(vec![parent.clone(), vec![Node::Cvasgn]]),
                    &node.name,
                    node.expression_l,
                    input,
                    0,
                )
                .map(|x| vec![x])
                .unwrap_or(vec![]),
                node.value
                    .as_ref()
                    .map(|value| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Cvasgn]]),
                            value,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),
            lib_ruby_parser::Node::Def(node) => itertools::concat(vec![
                self.search_node(
                    itertools::concat(vec![parent.clone(), vec![Node::Def]]),
                    &node.name,
                    node.name_l,
                    &self.input,
                    0,
                )
                .map(|node| vec![node])
                .unwrap_or(vec![]),
                node.args
                    .as_ref()
                    .map(|args| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Def]]),
                            args,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Def]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Defined(node) => self.search(
                itertools::concat(vec![parent, vec![Node::Defined]]),
                &node.value,
                input,
            ),

            lib_ruby_parser::Node::Defs(node) => itertools::concat(vec![
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Defs]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.args
                    .as_ref()
                    .map(|args| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Defs]]),
                            args,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search_node(
                    itertools::concat(vec![parent, vec![Node::Defs]]),
                    &node.name,
                    node.name_l,
                    &self.input,
                    0,
                )
                .map(|x| vec![x])
                .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Dstr(node) => node
                .parts
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Dstr]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Dsym(node) => node
                .parts
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Dsym]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::EFlipFlop(node) => itertools::concat(vec![
                node.left
                    .as_ref()
                    .map(|left| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::EFlipFlop]]),
                            left,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.right
                    .as_ref()
                    .map(|right| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::EFlipFlop]]),
                            right,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::EmptyElse(node) => self
                .matcher
                .is_match("else")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::EmptyElse]]),
                                pos.1,
                                pos.1 + "else".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Encoding(node) => self
                .matcher
                .is_match("__ENCODING__")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Encoding]]),
                                pos.1,
                                pos.1 + "__ENCODING__".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Ensure(node) => itertools::concat(vec![
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Ensure]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.ensure
                    .as_ref()
                    .map(|ensure| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Ensure]]),
                            ensure,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Erange(node) => itertools::concat(vec![
                node.left
                    .as_ref()
                    .map(|left| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Erange]]),
                            left,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.right
                    .as_ref()
                    .map(|right| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Erange]]),
                            right,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::False(node) => self
                .matcher
                .is_match("false")
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::False]]),
                            pos.1,
                            pos.1 + "false".len(),
                        )
                    }) {
                        Some(result) => vec![result],
                        None => vec![],
                    }
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::File(node) => self
                .matcher
                .is_match("__FILE__")
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::File]]),
                            pos.1,
                            pos.1 + "__FILE__".len(),
                        )
                    }) {
                        Some(result) => vec![result],
                        None => vec![],
                    }
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::FindPattern(node) => node
                .elements
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::FindPattern]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Float(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::Float]]),
                    &node.value,
                    node.expression_l,
                    input,
                    0,
                )
                .map(|r| vec![r])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::For(node) => itertools::concat(vec![
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::For]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::For]]),
                    &node.iterator,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::For]]),
                    &node.iteratee,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::ForwardArg(node) => self
                .matcher
                .is_match("...")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::ForwardArg]]),
                                pos.1,
                                pos.1 + "...".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::ForwardedArgs(node) => self
                .matcher
                .is_match("...")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::ForwardArg]]),
                                pos.1,
                                pos.1 + "...".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Gvar(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Gvar]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Gvasgn(node) => itertools::concat(vec![
                node.value
                    .as_ref()
                    .map(|v| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Gvasgn]]),
                            v,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search_node(
                    itertools::concat(vec![parent, vec![Node::Gvasgn]]),
                    &node.name.to_string(),
                    node.expression_l,
                    input,
                    0,
                )
                .map(|r| vec![r])
                .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Hash(node) => node
                .pairs
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Hash]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::HashPattern(node) => node
                .elements
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::HashPattern]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Heredoc(node) => node
                .parts
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Heredoc]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::IFlipFlop(node) => itertools::concat(vec![
                node.left
                    .as_ref()
                    .map(|left| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::IFlipFlop]]),
                            left,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.right
                    .as_ref()
                    .map(|right| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::IFlipFlop]]),
                            right,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::If(node) => itertools::concat(vec![
                node.if_true
                    .as_ref()
                    .map(|if_true| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::If]]),
                            if_true,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.if_false
                    .as_ref()
                    .map(|if_false| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::If]]),
                            if_false,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search(
                    itertools::concat(vec![parent, vec![Node::If]]),
                    &node.cond,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::IfGuard(node) => self.search(
                itertools::concat(vec![parent, vec![Node::IfGuard]]),
                &node.cond,
                input,
            ),

            lib_ruby_parser::Node::IfMod(node) => itertools::concat(vec![
                node.if_true
                    .as_ref()
                    .map(|if_true| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::IfMod]]),
                            if_true,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.if_false
                    .as_ref()
                    .map(|if_false| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::IfMod]]),
                            if_false,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search(
                    itertools::concat(vec![parent, vec![Node::IfMod]]),
                    &node.cond,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::IfTernary(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::IfTernary]]),
                    &node.cond,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::IfTernary]]),
                    &node.if_true,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::IfTernary]]),
                    &node.if_false,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::InPattern(node) => itertools::concat(vec![
                node.guard
                    .as_ref()
                    .map(|guard| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::InPattern]]),
                            guard,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::InPattern]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search(
                    itertools::concat(vec![parent, vec![Node::InPattern]]),
                    &node.pattern,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Index(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Index]]),
                    &node.recv,
                    input,
                ),
                node.indexes
                    .iter()
                    .flat_map(|statement| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Index]]),
                            statement,
                            input,
                        )
                    })
                    .collect(),
            ]),

            lib_ruby_parser::Node::IndexAsgn(node) => itertools::concat(vec![
                node.value
                    .as_ref()
                    .map(|value| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::IndexAsgn]]),
                            value,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::IndexAsgn]]),
                    &node.recv,
                    input,
                ),
                node.indexes
                    .iter()
                    .flat_map(|statement| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::IndexAsgn]]),
                            statement,
                            input,
                        )
                    })
                    .collect(),
            ]),

            lib_ruby_parser::Node::Int(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::Int]]),
                    &node.value,
                    node.expression_l,
                    input,
                    0,
                )
                .map(|x| vec![x])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Irange(node) => itertools::concat(vec![
                node.left
                    .as_ref()
                    .map(|left| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Irange]]),
                            left,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.right
                    .as_ref()
                    .map(|right| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Irange]]),
                            right,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Ivar(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::Ivar]]),
                            pos.1,
                            pos.1 + node.name.len(),
                        )
                    }) {
                        Some(result) => vec![result],
                        None => vec![],
                    }
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Ivasgn(node) => itertools::concat(vec![
                self.search_node(
                    itertools::concat(vec![parent.clone(), vec![Node::Ivasgn]]),
                    &node.name,
                    node.name_l,
                    input,
                    0,
                )
                .map(|x| vec![x])
                .unwrap_or(vec![]),
                node.value
                    .as_ref()
                    .map(|value| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Ivasgn]]),
                            value,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::KwBegin(node) => node
                .statements
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::KwBegin]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Kwarg(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::Kwarg]]),
                            pos.1,
                            pos.1 + node.name.len(),
                        )
                    }) {
                        Some(result) => vec![result],
                        None => vec![],
                    }
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Kwargs(node) => node
                .pairs
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Kwargs]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Kwnilarg(node) => self
                .matcher
                .is_match("**nil")
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::Kwnilarg]]),
                            pos.1,
                            pos.1 + "**nil".len(),
                        )
                    }) {
                        Some(result) => vec![result],
                        None => vec![],
                    }
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Kwoptarg(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    match input.line_col_for_pos(node.expression_l.begin).map(|pos| {
                        LineResult::new(
                            pos.0,
                            self.lines[pos.0].clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::Kwoptarg]]),
                            pos.1,
                            pos.1 + node.name.len(),
                        )
                    }) {
                        Some(result) => itertools::concat(vec![
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::Kwoptarg]]),
                                &node.default,
                                input,
                            ),
                            vec![result],
                        ]),
                        None => self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Kwoptarg]]),
                            &node.default,
                            input,
                        ),
                    }
                })
                .unwrap_or(self.search(
                    itertools::concat(vec![parent, vec![Node::Kwoptarg]]),
                    &node.default,
                    input,
                )),

            lib_ruby_parser::Node::Kwrestarg(node) => match &node.name {
                Some(name) => self
                    .matcher
                    .is_match(name)
                    .then(|| {
                        input
                            .line_col_for_pos(node.expression_l.begin)
                            .map(|pos| {
                                LineResult::new(
                                    pos.0,
                                    self.lines[pos.0].clone(),
                                    itertools::concat(vec![parent.clone(), vec![Node::Kwrestarg]]),
                                    pos.1,
                                    pos.1 + name.len(),
                                )
                            })
                            .map(|r| vec![r])
                            .unwrap_or(vec![])
                    })
                    .unwrap_or(vec![]),
                None => vec![],
            },

            lib_ruby_parser::Node::Kwsplat(node) => self.search(
                itertools::concat(vec![parent, vec![Node::Kwsplat]]),
                &node.value,
                input,
            ),

            lib_ruby_parser::Node::Lambda(node) => self
                .matcher
                .is_match("->")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Lambda]]),
                                pos.1,
                                pos.1 + "->".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Line(node) => self
                .matcher
                .is_match("__LINE__")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Line]]),
                                pos.1,
                                pos.1 + "__LINE__".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Lvar(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Lvar]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Lvasgn(node) => {
                if self.matcher.is_match(&node.name) {
                    input
                        .line_col_for_pos(node.name_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Lvasgn]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(
                            node.value
                                .as_ref()
                                .map(|value| {
                                    self.search(
                                        itertools::concat(vec![parent, vec![Node::Lvasgn]]),
                                        value,
                                        input,
                                    )
                                })
                                .unwrap_or(vec![]),
                        )
                } else {
                    node.value
                        .as_ref()
                        .map(|value| {
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::Lvasgn]]),
                                value,
                                input,
                            )
                        })
                        .unwrap_or(vec![])
                }
            }

            lib_ruby_parser::Node::Masgn(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Masgn]]),
                    &node.lhs,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::Masgn]]),
                    &node.rhs,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::MatchAlt(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::MatchAlt]]),
                    &node.lhs,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::MatchAlt]]),
                    &node.rhs,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::MatchAs(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::MatchAs]]),
                    &node.value,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::MatchAs]]),
                    &node.as_,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::MatchCurrentLine(node) => self.search(
                itertools::concat(vec![parent, vec![Node::MatchCurrentLine]]),
                &node.re,
                input,
            ),

            lib_ruby_parser::Node::MatchNilPattern(node) => self
                .matcher
                .is_match("**nil")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![
                                    parent.clone(),
                                    vec![Node::MatchNilPattern],
                                ]),
                                pos.1,
                                pos.1 + "**nil".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::MatchPattern(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::MatchPattern]]),
                    &node.value,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::MatchPattern]]),
                    &node.pattern,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::MatchPatternP(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::MatchPatternP]]),
                    &node.value,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::MatchPatternP]]),
                    &node.pattern,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::MatchRest(node) => node
                .name
                .as_ref()
                .map(|name| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::MatchRest]]),
                        name,
                        input,
                    )
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::MatchVar(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::MatchVar]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::MatchWithLvasgn(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::MatchWithLvasgn]]),
                    &node.re,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::MatchWithLvasgn]]),
                    &node.value,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Mlhs(node) => itertools::concat(node.items.iter().map(|arg| {
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Mlhs]]),
                    arg,
                    input,
                )
            })),

            lib_ruby_parser::Node::Module(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Module]]),
                    &node.name,
                    input,
                ),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Module]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Next(node) => itertools::concat(vec![
                self.matcher
                    .is_match("next")
                    .then(|| {
                        input
                            .line_col_for_pos(node.expression_l.begin)
                            .map(|pos| {
                                LineResult::new(
                                    pos.0,
                                    self.lines[pos.0].clone(),
                                    itertools::concat(vec![parent.clone(), vec![Node::Next]]),
                                    pos.1,
                                    pos.1 + "next".len(),
                                )
                            })
                            .map(|r| vec![r])
                            .unwrap_or(vec![])
                    })
                    .unwrap_or(vec![]),
                node.args
                    .iter()
                    .flat_map(|arg| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Next]]),
                            arg,
                            input,
                        )
                    })
                    .collect::<Vec<LineResult>>(),
            ]),

            lib_ruby_parser::Node::Nil(node) => self
                .matcher
                .is_match("nil")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Nil]]),
                                pos.1,
                                pos.1 + "nil".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::NthRef(node) => self
                .matcher
                .is_match(format!("${}", &node.name).as_str())
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::NthRef]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Numblock(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Numblock]]),
                    &node.call,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Numblock]]),
                    &node.body,
                    input,
                ),
                self.matcher
                    .is_match(format!("_{}", node.numargs).as_str())
                    .then(|| {
                        input
                            .line_col_for_pos(node.begin_l.begin)
                            .map(|pos| {
                                LineResult::new(
                                    pos.0,
                                    self.lines[pos.0].clone(),
                                    itertools::concat(vec![parent.clone(), vec![Node::Numblock]]),
                                    pos.1,
                                    pos.1 + "nil".len(),
                                )
                            })
                            .map(|r| vec![r])
                            .unwrap_or(vec![])
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::OpAsgn(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::OpAsgn]]),
                    &node.recv,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::OpAsgn]]),
                    &node.value,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Optarg(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    input
                        .line_col_for_pos(node.name_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Optarg]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Or(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Or]]),
                    &node.lhs,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::Or]]),
                    &node.rhs,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::OrAsgn(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::OrAsgn]]),
                    &node.recv,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::OrAsgn]]),
                    &node.value,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Pair(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Pair]]),
                    &node.key,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::Pair]]),
                    &node.value,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::Pin(node) => self.search(
                itertools::concat(vec![parent, vec![Node::Pin]]),
                &node.var,
                input,
            ),

            lib_ruby_parser::Node::Postexe(node) => itertools::concat(vec![
                self.matcher
                    .is_match("END")
                    .then(|| {
                        input
                            .line_col_for_pos(node.expression_l.begin)
                            .map(|pos| {
                                LineResult::new(
                                    pos.0,
                                    self.lines[pos.0].clone(),
                                    itertools::concat(vec![parent.clone(), vec![Node::Postexe]]),
                                    pos.1,
                                    pos.1 + "END".len(),
                                )
                            })
                            .map(|r| vec![r])
                            .unwrap_or(vec![])
                    })
                    .unwrap_or(vec![]),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Postexe]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Preexe(node) => itertools::concat(vec![
                self.matcher
                    .is_match("BEGIN")
                    .then(|| {
                        input
                            .line_col_for_pos(node.expression_l.begin)
                            .map(|pos| {
                                LineResult::new(
                                    pos.0,
                                    self.lines[pos.0].clone(),
                                    itertools::concat(vec![parent.clone(), vec![Node::Preexe]]),
                                    pos.1,
                                    pos.1 + "BEGIN".len(),
                                )
                            })
                            .map(|r| vec![r])
                            .unwrap_or(vec![])
                    })
                    .unwrap_or(vec![]),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Preexe]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Procarg0(node) => node
                .args
                .iter()
                .flat_map(|arg| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Procarg0]]),
                        arg,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Rational(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::Rational]]),
                    &node.value,
                    node.expression_l,
                    input,
                    0,
                )
                .map(|r| vec![r])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Redo(node) => self
                .matcher
                .is_match("redo")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Redo]]),
                                pos.1,
                                pos.1 + "redo".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::RegOpt(node) => node
                .options
                .as_ref()
                .map(|options| {
                    self.matcher
                        .is_match(options)
                        .then(|| {
                            input
                                .line_col_for_pos(node.expression_l.begin)
                                .map(|pos| {
                                    LineResult::new(
                                        pos.0,
                                        self.lines[pos.0].clone(),
                                        itertools::concat(vec![parent.clone(), vec![Node::RegOpt]]),
                                        pos.1,
                                        pos.1 + options.len(),
                                    )
                                })
                                .map(|r| vec![r])
                                .unwrap_or(vec![])
                        })
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Regexp(node) => node
                .options
                .as_ref()
                .map(|options| {
                    itertools::concat(vec![
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Regexp]]),
                            options,
                            input,
                        ),
                        node.parts
                            .iter()
                            .flat_map(|arg| {
                                self.search(
                                    itertools::concat(vec![parent.clone(), vec![Node::Regexp]]),
                                    arg,
                                    input,
                                )
                            })
                            .collect(),
                    ])
                })
                .unwrap_or(
                    node.parts
                        .iter()
                        .flat_map(|arg| {
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::Regexp]]),
                                arg,
                                input,
                            )
                        })
                        .collect(),
                ),

            lib_ruby_parser::Node::Rescue(node) => itertools::concat(vec![
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Rescue]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.else_
                    .as_ref()
                    .map(|else_| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Rescue]]),
                            else_,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.rescue_bodies
                    .iter()
                    .flat_map(|statement| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Rescue]]),
                            statement,
                            input,
                        )
                    })
                    .collect(),
            ]),

            lib_ruby_parser::Node::RescueBody(node) => itertools::concat(vec![
                node.exc_list
                    .as_ref()
                    .map(|exc_list| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::RescueBody]]),
                            exc_list,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.exc_var
                    .as_ref()
                    .map(|exc_var| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::RescueBody]]),
                            exc_var,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
                node.body
                    .as_ref()
                    .map(|body| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::RescueBody]]),
                            body,
                            input,
                        )
                    })
                    .unwrap_or(vec![]),
            ]),

            lib_ruby_parser::Node::Restarg(node) => node
                .name
                .as_ref()
                .map(|s| {
                    self.matcher
                        .is_match(s)
                        .then(|| {
                            input
                                .line_col_for_pos(node.expression_l.begin)
                                .map(|pos| {
                                    LineResult::new(
                                        pos.0,
                                        self.lines[pos.0].clone(),
                                        itertools::concat(vec![
                                            parent.clone(),
                                            vec![Node::Restarg],
                                        ]),
                                        pos.1,
                                        pos.1 + s.len(),
                                    )
                                })
                                .map(|r| vec![r])
                                .unwrap_or(vec![])
                        })
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Retry(node) => self
                .matcher
                .is_match("retry")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Retry]]),
                                pos.1,
                                pos.1 + "retry".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Return(node) => itertools::concat(node.args.iter().map(|arg| {
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Return]]),
                    arg,
                    input,
                )
            })),

            lib_ruby_parser::Node::SClass(node) => node
                .body
                .as_ref()
                .map(|body| {
                    itertools::concat(vec![
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::SClass]]),
                            &node.expr,
                            input,
                        ),
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::SClass]]),
                            body,
                            input,
                        ),
                    ])
                })
                .unwrap_or(self.search(
                    itertools::concat(vec![parent, vec![Node::SClass]]),
                    &node.expr,
                    input,
                )),

            lib_ruby_parser::Node::Splat(node) => node
                .value
                .as_ref()
                .map(|value| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Splat]]),
                        value,
                        input,
                    )
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Self_(node) => self
                .matcher
                .is_match("self")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Self_]]),
                                pos.1,
                                pos.1 + "self".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Send(node) => self
                .search_node(
                    itertools::concat(vec![parent.clone(), vec![Node::Send]]),
                    &node.method_name,
                    node.selector_l.unwrap_or(node.expression_l),
                    input,
                    0,
                )
                .map(|r| {
                    itertools::concat(vec![
                        node.recv
                            .as_ref()
                            .map(|node| {
                                self.search(
                                    itertools::concat(vec![parent.clone(), vec![Node::Send]]),
                                    node,
                                    input,
                                )
                            })
                            .unwrap_or(vec![]),
                        vec![r],
                        node.args
                            .iter()
                            .flat_map(|statement| {
                                self.search(
                                    itertools::concat(vec![parent.clone(), vec![Node::Send]]),
                                    statement,
                                    input,
                                )
                            })
                            .collect(),
                    ])
                })
                .unwrap_or(itertools::concat(vec![
                    node.recv
                        .as_ref()
                        .map(|node| {
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::Send]]),
                                node,
                                input,
                            )
                        })
                        .unwrap_or(vec![]),
                    node.args
                        .iter()
                        .flat_map(|statement| {
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::Send]]),
                                statement,
                                input,
                            )
                        })
                        .collect(),
                ])),

            lib_ruby_parser::Node::Shadowarg(node) => self
                .matcher
                .is_match(&node.name)
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::Shadowarg]]),
                                pos.1,
                                pos.1 + node.name.len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Str(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::Str]]),
                    &node.value.to_string().unwrap_or("".to_string()),
                    node.expression_l,
                    input,
                    0,
                )
                .map(|r| vec![r])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Super(node) => itertools::concat(vec![
                self.matcher
                    .is_match("super")
                    .then(|| {
                        input
                            .line_col_for_pos(node.expression_l.begin)
                            .map(|pos| {
                                LineResult::new(
                                    pos.0,
                                    self.lines[pos.0].clone(),
                                    itertools::concat(vec![parent.clone(), vec![Node::Super]]),
                                    pos.1,
                                    pos.1 + "super".len(),
                                )
                            })
                            .map(|r| vec![r])
                            .unwrap_or(vec![])
                    })
                    .unwrap_or(vec![]),
                node.args
                    .iter()
                    .flat_map(|arg| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::Args]]),
                            arg,
                            input,
                        )
                    })
                    .collect(),
            ]),

            lib_ruby_parser::Node::Sym(node) => self
                .search_node(
                    itertools::concat(vec![parent, vec![Node::Sym]]),
                    &node.name.to_string().unwrap_or("".to_string()),
                    node.expression_l,
                    input,
                    1,
                )
                .map(|v| vec![v])
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::True(node) => self
                .matcher
                .is_match("true")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent, vec![Node::True]]),
                                pos.1,
                                pos.1 + "true".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),

            lib_ruby_parser::Node::Undef(node) => node
                .names
                .iter()
                .flat_map(|statement| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Undef]]),
                        statement,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::UnlessGuard(node) => self.search(
                itertools::concat(vec![parent, vec![Node::UnlessGuard]]),
                &node.cond,
                input,
            ),

            lib_ruby_parser::Node::Until(node) => match &node.body {
                Some(body) => itertools::concat(vec![
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::Until]]),
                        &node.cond,
                        input,
                    ),
                    self.search(
                        itertools::concat(vec![parent, vec![Node::Until]]),
                        body,
                        input,
                    ),
                ]),
                _ => self.search(
                    itertools::concat(vec![parent, vec![Node::Until]]),
                    &node.cond,
                    input,
                ),
            },

            lib_ruby_parser::Node::UntilPost(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::UntilPost]]),
                    &node.cond,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::UntilPost]]),
                    &node.body,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::When(node) => match &node.body {
                Some(body) => itertools::concat(vec![
                    node.patterns
                        .iter()
                        .flat_map(|arg| {
                            self.search(
                                itertools::concat(vec![parent.clone(), vec![Node::When]]),
                                arg,
                                input,
                            )
                        })
                        .collect(),
                    self.search(
                        itertools::concat(vec![parent, vec![Node::When]]),
                        body,
                        input,
                    ),
                ]),
                _ => node
                    .patterns
                    .iter()
                    .flat_map(|arg| {
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::When]]),
                            arg,
                            input,
                        )
                    })
                    .collect(),
            },

            lib_ruby_parser::Node::While(node) => node
                .body
                .as_ref()
                .map(|body| {
                    itertools::concat(vec![
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::While]]),
                            &node.cond,
                            input,
                        ),
                        self.search(
                            itertools::concat(vec![parent.clone(), vec![Node::While]]),
                            body,
                            input,
                        ),
                    ])
                })
                .unwrap_or(self.search(
                    itertools::concat(vec![parent, vec![Node::While]]),
                    &node.cond,
                    input,
                )),

            lib_ruby_parser::Node::WhilePost(node) => itertools::concat(vec![
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::WhilePost]]),
                    &node.cond,
                    input,
                ),
                self.search(
                    itertools::concat(vec![parent, vec![Node::WhilePost]]),
                    &node.body,
                    input,
                ),
            ]),

            lib_ruby_parser::Node::XHeredoc(node) => node
                .parts
                .iter()
                .flat_map(|arg| {
                    self.search(
                        itertools::concat(vec![parent.clone(), vec![Node::XHeredoc]]),
                        arg,
                        input,
                    )
                })
                .collect(),

            lib_ruby_parser::Node::Xstr(node) => itertools::concat(node.parts.iter().map(|arg| {
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Xstr]]),
                    arg,
                    input,
                )
            })),

            lib_ruby_parser::Node::Yield(node) => itertools::concat(node.args.iter().map(|arg| {
                self.search(
                    itertools::concat(vec![parent.clone(), vec![Node::Yield]]),
                    arg,
                    input,
                )
            })),

            lib_ruby_parser::Node::ZSuper(node) => self
                .matcher
                .is_match("super")
                .then(|| {
                    input
                        .line_col_for_pos(node.expression_l.begin)
                        .map(|pos| {
                            LineResult::new(
                                pos.0,
                                self.lines[pos.0].clone(),
                                itertools::concat(vec![parent.clone(), vec![Node::ZSuper]]),
                                pos.1,
                                pos.1 + "super".len(),
                            )
                        })
                        .map(|r| vec![r])
                        .unwrap_or(vec![])
                })
                .unwrap_or(vec![]),
        }
    }

    fn search_node(
        &self,
        nodes: Vec<Node>,
        text: &str,
        loc: Loc,
        input: &DecodedInput,
        offset: usize,
    ) -> Option<LineResult> {
        if self.matcher.is_match(text) {
            input.line_col_for_pos(loc.begin).map(|pos| {
                LineResult::new(
                    pos.0,
                    self.lines[pos.0].clone(),
                    nodes,
                    pos.1,
                    if pos.1 + text.len() + offset < self.lines[pos.0].clone().len() {
                        pos.1 + text.len() + offset
                    } else {
                        self.lines[pos.0].clone().len()
                    },
                )
            })
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::matcher::TextMatcher;
    use rstest::rstest;

    fn line_result(
        line: String,
        row: usize,
        column_start: usize,
        column_end: usize,
        nodes: Vec<Node>,
    ) -> LineResult {
        LineResult {
            line,
            row,
            nodes,
            column_start,
            column_end,
        }
    }

    #[rstest]
    // class
    #[case("Class", None, line_result("class Class < vvv; end".to_string(), 0, 6, 11, vec![Node::Class]))]
    #[case("Class2", None, line_result("class Class < vvv; class Class2; end; end".to_string(), 0, 25, 31, vec![Node::Class, Node::Class]))]
    #[case("SingletonClass", None, line_result("class << SingletonClass; def test; end end".to_string(), 0, 9, 23, vec![Node::SClass]))]
    // module
    #[case("Module", None, line_result("module Module; def test; end; end".to_string(), 0, 7, 13, vec![Node::Module]))]
    // def
    #[case("def_test", None, line_result("def def_test; end".to_string(), 0, 4, 12, vec![Node::Def]))]
    #[case("def_test", None, line_result("def def_test; puts 'bar'; end".to_string(), 0, 4, 12, vec![Node::Def]))]
    #[case("rest_test", None, line_result("def m(*rest_test); end".to_string(), 0, 6, 15, vec![Node::Def, Node::Arg, Node::Restarg]))]
    #[case("undef_test", None, line_result("undef undef_test, :test".to_string(), 0, 6, 17, vec![Node::Undef, Node::Sym]))]
    #[case("foo", None, line_result("def x.foo(args); puts 'v'; end".to_string(), 0, 6, 9, vec![Node::Defs]))]
    #[case("foo", None, line_result("def m(**foo); end".to_string(), 0, 6, 9, vec![Node::Def, Node::Arg, Node::Kwrestarg]))]
    #[case("foo", None, line_result("def m(foo: 1); end".to_string(), 0, 6, 9, vec![Node::Def, Node::Arg, Node::Kwoptarg]))]
    #[case("1", None, line_result("def m(foo: 1); end".to_string(), 0, 11, 12, vec![Node::Def, Node::Arg, Node::Kwoptarg, Node::Int]))]
    #[case("nil", None, line_result("def m(**nil); end".to_string(), 0, 6, 11, vec![Node::Def, Node::Arg, Node::Kwnilarg]))]
    #[case("bar", None, line_result("def foo(bar:); end".to_string(), 0, 8, 11, vec![Node::Def, Node::Arg, Node::Kwarg]))]
    #[case("...", None, line_result("def m(...); end".to_string(), 0, 6, 9, vec![Node::Def, Node::Arg, Node::ForwardArg]))]
    // sym
    #[case("sym", None, line_result("var.try(:sym)".to_string(), 0, 8, 12, vec![Node::Send, Node::Sym]))]
    #[case("foo", None, line_result(":\"#{foo}\"".to_string(), 0, 4, 7, vec![Node::Dsym, Node::Begin, Node::Send]))]
    // alias
    #[case("alias_test", None, line_result("alias :alias_test :new_alias".to_string(), 0, 6, 17, vec![Node::Alias, Node::Sym]))]
    // local var
    #[case("local_var_test", None, line_result("local_var_test = 2 + 2".to_string(), 0, 0, 14, vec![Node::Lvasgn]))]
    // send
    #[case("send_test", None, line_result("var.send_test()".to_string(), 0, 4, 13, vec![Node::Send]))]
    #[case("var", None, line_result("var.send_test()".to_string(), 0, 0, 3, vec![Node::Send, Node::Send]))]
    #[case("local_var_test", None, line_result("local_var_test = 2 + 2".to_string(), 0, 0, 14, vec![Node::Lvasgn]))]
    #[case("op_assign_test", None, line_result("op_assign_test += 1".to_string(), 0, 0, 14, vec![Node::OpAsgn, Node::Lvasgn]))]
    #[case("or_assign_test", None, line_result("or_assign_test ||= 1".to_string(), 0, 0, 14, vec![Node::OrAsgn, Node::Lvasgn]))]
    #[case("mass_assign_test", None, line_result("mass_assign_test, test = 1, 2".to_string(), 0, 0, 16, vec![Node::Masgn, Node::Mlhs, Node::Lvasgn]))]
    // while
    #[case("while_test",  None, line_result("while while_test do; test; end".to_string(), 0, 6, 16, vec![Node::While, Node::Send]))]
    #[case("while_post_test", None, line_result("begin while_post_test; end while test".to_string(), 0, 6, 21, vec![Node::WhilePost, Node::KwBegin, Node::Send]))]
    // until
    #[case("until_test", None, line_result("until until_test do; test; end".to_string(), 0, 6, 16, vec![Node::Until, Node::Send]))]
    #[case("bar", None, line_result("until foo do; bar; end".to_string(), 0, 14, 17, vec![Node::Until, Node::Send]))]
    #[case("unless_test", None, line_result("puts 'test' unless unless_test".to_string(), 0, 19, 30, vec![Node::IfMod, Node::Send]))]
    #[case("rescue_test", None, line_result("begin; test; rescue StandardError => rescue_test; true_test; else; else_test; end".to_string(), 0, 37, 48, vec![Node::KwBegin, Node::Rescue, Node::RescueBody, Node::Lvasgn]))]
    #[case("bar", None, line_result("foo(**bar)".to_string(), 0, 6, 9, vec![Node::Send, Node::Kwargs, Node::Kwsplat, Node::Send]))]
    #[case("regex_test", None, line_result("/regex_test/".to_string(), 0, 1, 11, vec![Node::Regexp, Node::Str]))]
    #[case("pin_test", None, line_result("pin_test = 1; case foo; in ^pin_test; end".to_string(), 0, 28, 36, vec![Node::Begin, Node::CaseMatch, Node::InPattern, Node::Pin, Node::Lvar]))]
    #[case("global_test", None, line_result("$global_test = 1000".to_string(), 0, 0, 12, vec![Node::Gvasgn]))]
    // const
    #[case("CONST", None, line_result("CONST = 1".to_string(), 0, 0, 5, vec![Node::Casgn]))]
    #[case("VAR", None, line_result("VAR::B = 1".to_string(), 0, 0, 3, vec![Node::Casgn]))]
    #[case("1", None, line_result("VAR::B = 1".to_string(), 0, 9, 10, vec![Node::Casgn, Node::Int]))]
    // instance var
    #[case("foo", None, line_result("@foo".to_string(), 0, 0, 4, vec![Node::Ivar]))]
    #[case("foo", None, line_result("@foo = 1".to_string(), 0, 0, 4, vec![Node::Ivasgn]))]
    #[case("1", None, line_result("@foo = 1".to_string(), 0, 7, 8, vec![Node::Ivasgn, Node::Int]))]
    // if
    #[case("foo", None, line_result("if foo; bar; end".to_string(), 0, 3, 6, vec![Node::If, Node::Send]))]
    #[case("bar", None, line_result("if foo; else bar; end".to_string(), 0, 13, 16, vec![Node::If, Node::Send]))]
    #[case("if_test", None, line_result("if if_test...bar_test; end".to_string(), 0, 3, 10, vec![Node::If, Node::EFlipFlop, Node::Send]))]
    #[case("test_cond", None, line_result("test_cond ? test_if_true : test_if_false".to_string(), 0, 0, 9, vec![Node::IfTernary, Node::Send]))]
    #[case("foo", None, line_result("if /foo/; end".to_string(), 0, 4, 7, vec![Node::If, Node::MatchCurrentLine, Node::Regexp, Node::Str]))]
    #[case("bar", None, line_result("case foo; in pattern if bar; end".to_string(), 0, 24, 27, vec![Node::CaseMatch, Node::InPattern, Node::IfGuard, Node::Send]))]
    // index
    #[case("foo", None, line_result("foo[1,2,3]".to_string(), 0, 0, 3, vec![Node::Index, Node::Send]))]
    #[case("foo", None, line_result("foo[1, 2, 3] = bar".to_string(), 0, 0, 3, vec![Node::IndexAsgn, Node::Send]))]
    // hash
    #[case("hash_test", None, line_result("test = { hash_test: 42 }".to_string(), 0, 9, 19, vec![Node::Lvasgn, Node::Hash, Node::Pair, Node::Sym]))]
    // class var
    #[case("foo", None, line_result("@@foo".to_string(), 0, 0, 5, vec![Node::Cvar]))]
    #[case("foo", None, line_result("@@foo = 1".to_string(), 0, 0, 5, vec![Node::Cvasgn]))]
    #[case("1", None, line_result("@@foo = 1".to_string(), 0, 8, 9, vec![Node::Cvasgn, Node::Int]))]
    // global var
    #[case("foo", None, line_result("$foo".to_string(), 0, 0, 4, vec![Node::Gvar]))]
    #[case("foo", None, line_result("$foo = 1".to_string(), 0, 0, 4, vec![Node::Gvasgn]))]
    #[case("1", None, line_result("$foo = 1".to_string(), 0, 7, 8, vec![Node::Gvasgn, Node::Int]))]
    // case
    #[case("case_test", None, line_result("case case_test; when test; end".to_string(), 0, 5, 14, vec![Node::Case, Node::Send]))]
    #[case("when_test", None, line_result("case test; when when_test; end".to_string(), 0, 16, 25, vec![Node::Case, Node::When, Node::Send]))]
    #[case("case_in_test", None, line_result("case foo; in *case_in_test; puts 'v' end".to_string(), 0, 14, 26, vec![Node::CaseMatch, Node::InPattern, Node::ArrayPattern, Node::MatchRest, Node::MatchVar]))]
    #[case("else_test", None, line_result("case 1; when 1; v; else else_test; end".to_string(), 0, 24, 33, vec![Node::Case, Node::Send]))]
    #[case("else_match_test", None, line_result("case 1; in 2; else else_match_test; end".to_string(), 0, 19, 34, vec![Node::CaseMatch, Node::Send]))]
    #[case("Foo", None, line_result("case 1; in Foo(42); end".to_string(), 0, 11, 14, vec![Node::CaseMatch, Node::InPattern, Node::ConstPattern]))]
    #[case("bar", None, line_result("case foo; in [*x, 1 => bar, *y]; end".to_string(), 0, 23, 26, vec![Node::CaseMatch, Node::InPattern, Node::FindPattern, Node::MatchAs, Node::MatchVar]))]
    #[case("1", None, line_result("case foo; in [*x, 1 => bar, *y]; end".to_string(), 0, 18, 19, vec![Node::CaseMatch, Node::InPattern, Node::FindPattern, Node::MatchAs, Node::Int]))]
    // proc
    #[case("proc_test", None, line_result("proc_test = ->(word) { puts word }".to_string(), 0, 0, 9, vec![Node::Lvasgn]))]
    #[case("->", None, line_result("proc_test = ->(word) { puts word }".to_string(), 0, 12, 14, vec![Node::Lvasgn, Node::Block, Node::Lambda]))]
    // int
    #[case("10", None, line_result("int_test = 10".to_string(), 0, 11, 13, vec![Node::Lvasgn, Node::Int]))]
    // float
    #[case("1.1", None, line_result("foo = 1.1".to_string(), 0, 6, 9, vec![Node::Lvasgn, Node::Float]))]
    // rational
    #[case("-1r", None, line_result("rational_test = -1r".to_string(), 0, 16, 19, vec![Node::Lvasgn, Node::Rational]))]
    // block pass
    #[case("block_test", None, line_result("foo(&block_test)".to_string(), 0, 5, 15, vec![Node::Send, Node::BlockPass, Node::Send]))]
    // block args
    #[case("foo", None, line_result("def m(&foo); end".to_string(), 0, 6, 9, vec![Node::Def, Node::Arg, Node::Blockarg]))]
    // break
    #[case("break_test", None, line_result("break :break_test".to_string(), 0, 6, 17, vec![Node::Break, Node::Sym]))]
    // csend
    #[case("csend_test", None, line_result("foo&.csend_test(42)".to_string(), 0, 5, 15, vec![Node::CSend]))]
    // super
    #[case("super", None, line_result("super".to_string(), 0, 0, 5, vec![Node::ZSuper]))]
    // xstr
    #[case("xstr_test", None, line_result("`sh #{xstr_test}`".to_string(), 0, 6, 15, vec![Node::Xstr, Node::Begin, Node::Send]))]
    // yield
    #[case("yield_test", None, line_result("yield yield_test, foo".to_string(), 0, 6, 16, vec![Node::Yield, Node::Send]))]
    // true
    #[case("true", None, line_result("value = true".to_string(), 0, 8, 12, vec![Node::Lvasgn, Node::True]))]
    // super
    #[case("super", None, line_result("super(1, 2)".to_string(), 0, 0, 5, vec![Node::Super]))]
    // shadowarg
    #[case("shadow", None, line_result("proc { |;shadow|}".to_string(), 0, 9, 15, vec![Node::Block, Node::Arg, Node::Shadowarg]))]
    // self
    #[case("self", None, line_result("self.vvvv".to_string(), 0, 0, 4, vec![Node::Send, Node::Self_]))]
    // splat
    #[case("splat", None, line_result("foo(*splat)".to_string(), 0, 5, 10, vec![Node::Send, Node::Splat, Node::Send]))]
    // return
    #[case("ret", None, line_result("return ret, 1".to_string(), 0, 7, 10, vec![Node::Return, Node::Send]))]
    // retry
    #[case("retry", None, line_result("retry if try < vv".to_string(), 0, 0, 5, vec![Node::IfMod, Node::Retry]))]
    // regexp
    #[case("regex", None, line_result("/regex/mix".to_string(), 0, 1, 6, vec![Node::Regexp, Node::Str]))]
    #[case("imx", None, line_result("/regex/mix".to_string(), 0, 7, 10, vec![Node::Regexp, Node::RegOpt]))]
    // redo
    #[case("redo", None, line_result("redo if test".to_string(), 0, 0, 4, vec![Node::IfMod, Node::Redo]))]
    // proc
    #[case("proc1", None, line_result("proc { |(proc1, proc2)| }".to_string(), 0, 9, 14, vec![Node::Block, Node::Arg, Node::Procarg0, Node::Arg]))]
    // preexe
    #[case("BEGIN", None, line_result("BEGIN { 1 }".to_string(), 0, 0, 5, vec![Node::Preexe]))]
    // postexe
    #[case("END", None, line_result("END { 1 }".to_string(), 0, 0, 3, vec![Node::Postexe]))]
    // and
    #[case("bar", None, line_result("foo && bar".to_string(), 0, 7, 10, vec![Node::And, Node::Send]))]
    #[case("foo", None, line_result("foo &&= bar".to_string(), 0, 0, 3, vec![Node::AndAsgn, Node::Lvasgn]))]
    // or
    #[case("bar", None, line_result("foo || bar".to_string(), 0, 7, 10, vec![Node::Or, Node::Send]))]
    // optarg
    #[case("bar", None, line_result("def foo(bar = 1); end".to_string(), 0, 8, 11, vec![Node::Def, Node::Arg, Node::Optarg]))]
    // num block
    #[case("_2", None, line_result("proc { _2 }".to_string(), 0, 5, 8, vec![Node::Numblock]))]
    // nthref
    #[case("$1", None, line_result("puts \"#$1\"".to_string(), 0, 7, 8, vec![Node::Send, Node::Dstr, Node::NthRef]))]
    // nil
    #[case("nil", None, line_result("v = nil".to_string(), 0, 4, 7, vec![Node::Lvasgn, Node::Nil]))]
    // next
    #[case("next", None, line_result("next 1".to_string(), 0, 0, 4, vec![Node::Next]))]
    // backref
    #[case("$+", None, line_result("$1, $+".to_string(), 0, 4, 6, vec![Node::BackRef]))]
    // cbase
    #[case("::", None, line_result("::X = 10".to_string(), 0, 0, 2, vec![Node::Casgn, Node::Cbase]))]
    // complex
    #[case("4i", None, line_result("3 + 4i".to_string(), 0, 4, 6, vec![Node::Send, Node::Complex]))]
    // defined
    #[case("foo", None, line_result("defined?(foo)".to_string(), 0, 9, 12, vec![Node::Defined, Node::Send]))]
    // empty else
    #[case("else", None, line_result("case foo; in 1; else; end".to_string(), 0, 16, 20, vec![Node::CaseMatch, Node::EmptyElse]))]
    // __ENCODING__
    #[case("__ENCODING__", None, line_result("__ENCODING__".to_string(), 0, 0, 12, vec![Node::Encoding]))]
    // __FILE__
    #[case("__FILE__", None, line_result("__FILE__".to_string(), 0, 0, 8, vec![Node::File]))]
    // __LINE__
    #[case("__LINE__", None, line_result("__LINE__".to_string(), 0, 0, 8, vec![Node::Line]))]
    // ensure
    #[case("bar", None, line_result("begin; foo; ensure; bar; end".to_string(), 0, 20, 23, vec![Node::KwBegin, Node::Ensure, Node::Send]))]
    // erange
    #[case("1", None, line_result("1...3".to_string(), 0, 0, 1, vec![Node::Erange, Node::Int]))]
    #[case("2", None, line_result("2..4".to_string(), 0, 0, 1, vec![Node::Irange, Node::Int]))]
    // IFlipFlop
    #[case("foo", None, line_result("if foo..bar; end".to_string(), 0, 3, 6, vec![Node::If, Node::IFlipFlop, Node::Send]))]
    #[case("bar", None, line_result("if foo..bar; end".to_string(), 0, 8, 11, vec![Node::If, Node::IFlipFlop, Node::Send]))]
    // false
    #[case("false", None, line_result("foo = false".to_string(), 0, 6, 11, vec![Node::Lvasgn, Node::False]))]
    // for
    #[case("foo", None, line_result("for foo in bar; puts 'v'; end".to_string(), 0, 4, 7, vec![Node::For, Node::Lvasgn]))]
    // match pattern
    #[case("foo", None, line_result("foo in pattern".to_string(), 0, 0, 3, vec![Node::MatchPatternP, Node::Send]))]
    #[case("foo", None, line_result("foo => pattern".to_string(), 0, 0, 3, vec![Node::MatchPattern, Node::Send]))]
    #[case("bar", None, line_result("foo in foo | bar".to_string(), 0, 13, 16, vec![Node::MatchPatternP, Node::MatchAlt, Node::MatchVar]))]
    #[case("nil", None, line_result("foo() in **nil".to_string(), 0, 9, 14, vec![Node::MatchPatternP, Node::HashPattern, Node::MatchNilPattern]))]
    // heredoc
    #[case("xhere_test", Some("<<-`HERE`\n  a   #{xhere_test} \nHERE".to_string()), line_result("  a   #{xhere_test} ".to_string(), 1, 8, 18, vec![Node::XHeredoc, Node::Begin, Node::Send]))]
    fn grep_source(
        #[case] query: String,
        #[case] text: Option<String>,
        #[case] expected: LineResult,
    ) {
        let m = TextMatcher::new(query, false, false);
        let source = Source::new(
            text.unwrap_or(expected.line.clone()).as_str(),
            &m,
            GrepOptions {
                start_nodes: None,
                end_nodes: None,
            },
        );

        match source.grep("").unwrap() {
            GrepResult::FileResult(r) => {
                assert_eq!(r.results.last().unwrap().row, expected.row);
                assert_eq!(
                    r.results.last().unwrap().column_start,
                    expected.column_start
                );
                assert_eq!(r.results.last().unwrap().column_end, expected.column_end);
                assert_eq!(r.results.last().unwrap().line, expected.line);
                assert_eq!(r.results.last().unwrap().nodes, expected.nodes);
            }
            GrepResult::FileErrorResult(_) => {
                panic!("failed")
            }
        };
    }

    #[rstest]
    // start_nodes
    #[case(
        "foo",
        "def test; foo.bar(); end;",
        Some(vec![Node::Def, Node::Send]),
        None,
        true
    )]
    #[case(
        "far",
        "def test; foo.bar(); end;",
        None,
        Some(vec![Node::Send]),
        false
    )]
    // end_nodes
    #[case(
        "foo",
        "def test; foo.bar(); end;",
        None,
        Some(vec![Node::Send, Node::Send]),
        true
    )]
    #[case(
        "test",
        "def test; foo.bar(); end;",
        None,
        Some(vec![Node::Send]),
        false
    )]
    fn grep_search_options(
        #[case] query: String,
        #[case] text: String,
        #[case] start_nodes: Option<Vec<Node>>,
        #[case] end_nodes: Option<Vec<Node>>,
        #[case] expected: bool,
    ) {
        let m = TextMatcher::new(query.to_string(), false, false);
        let source = Source::new(
            text.as_str(),
            &m,
            GrepOptions {
                start_nodes: start_nodes,
                end_nodes: end_nodes,
            },
        );

        let actual = source.grep("");
        assert_eq!(actual.is_some(), expected);
    }

    #[rstest]
    #[case(
        vec![Node::Begin, Node::Class],
        "Begin > Class",
    )]
    #[case(
        vec![Node::Module, Node::Def],
        "Module > Def",
    )]
    fn test_to_nodes_string(#[case] nodes: Vec<Node>, #[case] expected: String) {
        assert_eq!(
            LineResult::new(0, "".to_string(), nodes, 0, 0).to_nodes_string(),
            expected
        );
    }

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
        false,
        false,
        false,
        None,
        None,
        "class Test\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
        false,
        true,
        false,
        None,
        None,
        "file class Test\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
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
        vec![line_result("def test".to_string(), 1, 5, 8, vec![Node::Class])],
        false,
        true,
        true,
        Some(1),
        None,
        "file:1 class Test\nfile:2 def test\n--\n".to_string(),
    )]
    #[case(
        vec!["class Test"," def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![line_result("def test".to_string(), 1, 5, 8, vec![Node::Class])],
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
        vec![line_result("def test".to_string(), 1, 5, 8, vec![Node::Class])],
        false,
        true,
        true,
        None,
        Some(1),
        "file:2 def test\nfile:2 end\n--\n".to_string(),
    )]
    #[case(
        vec!["class Test", "def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![line_result("def test".to_string(), 1, 5, 8, vec![Node::Class])],
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
        vec![line_result("def test".to_string(), 1, 5, 8, vec![Node::Class, Node::Def])],
        true,
        false,
        false,
        None,
        None,
        "Class > Def\ndef test\n--\n".to_string(),
    )]
    #[case(
        vec!["class Test", "def test", "end"].iter().map(|x| x.to_string()).collect(),
        vec![line_result("def test".to_string(), 1, 5, 8, vec![Node::Class, Node::Def])],
        true,
        true,
        true,
        None,
        None,
        "Class > Def\nfile:2 def test\n--\n".to_string(),
    )]
    fn test_print_result(
        #[case] lines: Vec<String>,
        #[case] results: Vec<LineResult>,
        #[case] with_nodes: bool,
        #[case] with_filename: bool,
        #[case] with_lineno: bool,
        #[case] before_context: Option<usize>,
        #[case] after_context: Option<usize>,
        #[case] expected: String,
    ) {
        let result = FileResult {
            filename: "file".to_string(),
            lines,
            results,
        };

        assert_eq!(
            result.to_result_string(
                with_nodes,
                with_filename,
                with_lineno,
                "--".to_string(),
                before_context,
                after_context,
            ),
            expected
        );
    }

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
        false,
        "1\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
        true,
        "file:1\n".to_string(),
    )]
    fn test_print_count(
        #[case] lines: Vec<String>,
        #[case] results: Vec<LineResult>,
        #[case] with_filename: bool,
        #[case] expected: String,
    ) {
        let result = FileResult {
            filename: "file".to_string(),
            lines,
            results,
        };

        assert_eq!(result.to_count_string(with_filename), expected);
    }
}
