use crate::{
    matcher::Matcher,
    node::{Node, NodePath},
};
use colored::*;
use itertools::Itertools;
use lib_ruby_parser::{source::DecodedInput, Diagnostic, Loc, Parser, ParserOptions, ParserResult};
use serde::Serialize;
use std::{fmt, vec};

pub struct GrepOptions {
    pub start_nodes: Option<Vec<Node>>,
    pub end_nodes: Option<Vec<Node>>,
}

#[derive(Serialize)]
pub enum GrepResult {
    FileResult(FileResult),
    FileErrorResult(FileErrorResult),
}

#[derive(Debug, Serialize)]
pub struct LineErrorResult {
    message: String,
    #[serde(skip_serializing)]
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

#[derive(Debug, Serialize)]
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

#[derive(Debug, Serialize)]
pub struct FileResult {
    #[serde(rename(serialize = "fileName"))]
    pub filename: String,
    #[serde(skip_serializing)]
    pub lines: Vec<String>,
    pub results: Vec<LineResult>,
}

#[derive(Debug, Serialize)]
#[serde(rename_all = "camelCase")]
pub struct LineResult {
    pub line: String,
    pub row: usize,
    pub nodes: Vec<Node>,
    pub column_start: usize,
    pub column_end: usize,
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

    pub fn to_result_string(&self, only_matching: bool) -> String {
        let start_text = &self.line[..self.column_start];
        let match_text = &self.line[self.column_start..self.column_end];
        let end = if match_text.starts_with('"') || match_text.starts_with('\'') {
            self.column_end + 2
        } else {
            self.column_end
        };
        let match_text = &self.line[self.column_start..end];
        let end_text = &self.line[end..];

        if only_matching {
            format!("{}", match_text.red().bold())
        } else {
            format!("{}{}{}", start_text, match_text.red().bold(), end_text,)
        }
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Arg]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone().unwrap_or("".to_string()),
                    itertools::concat(vec![parent.clone(), vec![Node::Blockarg]]),
                ))
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
                .is_match(NodePath(
                    "::".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Cbase]]),
                ))
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
                .is_match(NodePath(
                    "else".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::EmptyElse]]),
                ))
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
                .is_match(NodePath(
                    "__ENCODING__".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Encoding]]),
                ))
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
                .is_match(NodePath(
                    "false".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::False]]),
                ))
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
                .is_match(NodePath(
                    "(__FILE__".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::File]]),
                ))
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
                .is_match(NodePath(
                    "...".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::ForwardArg]]),
                ))
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
                .is_match(NodePath(
                    "...".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::ForwardArg]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Gvar]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Ivar]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Kwarg]]),
                ))
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
                .is_match(NodePath(
                    "**nil".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Kwnilarg]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Kwoptarg]]),
                ))
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
                    .is_match(NodePath(
                        name.clone(),
                        itertools::concat(vec![parent.clone(), vec![Node::Kwrestarg]]),
                    ))
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
                .is_match(NodePath(
                    "->".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Lambda]]),
                ))
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
                .is_match(NodePath(
                    "__LINE__".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Line]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Lvar]]),
                ))
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
                if self.matcher.is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Lvasgn]]),
                )) {
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
                .is_match(NodePath(
                    "**nil".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::MatchNilPattern]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::MatchVar]]),
                ))
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
                    .is_match(NodePath(
                        "next".to_string(),
                        itertools::concat(vec![parent.clone(), vec![Node::Next]]),
                    ))
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
                .is_match(NodePath(
                    "nil".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Nil]]),
                ))
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
                .is_match(NodePath(
                    format!("${}", &node.name),
                    itertools::concat(vec![parent.clone(), vec![Node::NthRef]]),
                ))
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
                    .is_match(NodePath(
                        format!("_{}", node.numargs),
                        itertools::concat(vec![parent.clone(), vec![Node::Numblock]]),
                    ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Optarg]]),
                ))
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
                    .is_match(NodePath(
                        "END".to_string(),
                        itertools::concat(vec![parent.clone(), vec![Node::Postexe]]),
                    ))
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
                    .is_match(NodePath(
                        "BEGIN".to_string(),
                        itertools::concat(vec![parent.clone(), vec![Node::Preexe]]),
                    ))
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
                .is_match(NodePath(
                    "redo".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Redo]]),
                ))
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
                        .is_match(NodePath(
                            options.clone(),
                            itertools::concat(vec![parent.clone(), vec![Node::RegOpt]]),
                        ))
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
                        .is_match(NodePath(
                            s.to_string(),
                            itertools::concat(vec![parent.clone(), vec![Node::Restarg]]),
                        ))
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
                .is_match(NodePath(
                    "retry".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Retry]]),
                ))
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
                .is_match(NodePath(
                    "self".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::Self_]]),
                ))
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
                .is_match(NodePath(
                    node.name.clone(),
                    itertools::concat(vec![parent.clone(), vec![Node::Shadowarg]]),
                ))
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
                    .is_match(NodePath(
                        "super".to_string(),
                        itertools::concat(vec![parent.clone(), vec![Node::Super]]),
                    ))
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
                .is_match(NodePath(
                    "true".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::True]]),
                ))
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
                .is_match(NodePath(
                    "super".to_string(),
                    itertools::concat(vec![parent.clone(), vec![Node::ZSuper]]),
                ))
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
        if self
            .matcher
            .is_match(NodePath(text.to_string(), nodes.clone()))
        {
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

    #[rstest]
    // class
    #[case("Class", None, LineResult {line: "class Class < vvv; end".to_string(), row: 0, column_start: 6, column_end: 11, nodes: vec![Node::Class]})]
    #[case("Class2", None, LineResult {line: "class Class < vvv; class Class2; end; end".to_string(), row: 0, column_start: 25, column_end: 31, nodes: vec![Node::Class, Node::Class]})]
    #[case("SingletonClass", None, LineResult {line: "class << SingletonClass; def test; end end".to_string(), row: 0, column_start: 9, column_end: 23, nodes: vec![Node::SClass]})]
    // module
    #[case("Module", None, LineResult {line: "module Module; def test; end; end".to_string(), row: 0, column_start: 7, column_end: 13, nodes: vec![Node::Module]})]
    // def
    #[case("def_test", None, LineResult {line: "def def_test; end".to_string(), row: 0, column_start: 4, column_end: 12, nodes: vec![Node::Def]})]
    #[case("def_test", None, LineResult {line: "def def_test; puts 'bar'; end".to_string(), row: 0, column_start: 4, column_end: 12, nodes: vec![Node::Def]})]
    #[case("rest_test", None, LineResult {line: "def m(*rest_test); end".to_string(), row: 0, column_start: 6, column_end: 15, nodes: vec![Node::Def, Node::Arg, Node::Restarg]})]
    #[case("undef_test", None, LineResult {line: "undef undef_test, row: :test".to_string(), row: 0, column_start : 6, column_end : 17, nodes: vec![Node::Undef, Node::Sym]})]
    #[case("foo", None, LineResult {line: "def x.foo(args); puts 'v'; end".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Defs]})]
    #[case("foo", None, LineResult {line: "def m(**foo); end".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Def, Node::Arg, Node::Kwrestarg]})]
    #[case("foo", None, LineResult {line: "def m(foo: 1); end".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Def, Node::Arg, Node::Kwoptarg]})]
    #[case("1", None, LineResult {line: "def m(foo: 1); end".to_string(), row: 0, column_start: 11, column_end: 12, nodes: vec![Node::Def, Node::Arg, Node::Kwoptarg, Node::Int]})]
    #[case("nil", None, LineResult {line: "def m(**nil); end".to_string(), row: 0, column_start: 6, column_end: 11, nodes: vec![Node::Def, Node::Arg, Node::Kwnilarg]})]
    #[case("bar", None, LineResult {line: "def foo(bar:); end".to_string(), row: 0, column_start: 8, column_end: 11, nodes: vec![Node::Def, Node::Arg, Node::Kwarg]})]
    #[case("...", None, LineResult {line: "def m(...); end".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Def, Node::Arg, Node::ForwardArg]})]
    // sym
    #[case("sym", None, LineResult {line: "var.try(:sym)".to_string(), row: 0, column_start: 8, column_end: 12, nodes: vec![Node::Send, Node::Sym]})]
    #[case("foo", None, LineResult {line: ":\"#{foo}\"".to_string(), row: 0, column_start: 4, column_end: 7, nodes: vec![Node::Dsym, Node::Begin, Node::Send]})]
    // alias
    #[case("alias_test", None, LineResult {line: "alias :alias_test :new_alias".to_string(), row: 0, column_start: 6, column_end: 17, nodes: vec![Node::Alias, Node::Sym]})]
    // local var
    #[case("local_var_test", None, LineResult {line: "local_var_test = 2 + 2".to_string(), row: 0, column_start: 0, column_end: 14, nodes: vec![Node::Lvasgn]})]
    // send
    #[case("send_test", None, LineResult {line: "var.send_test()".to_string(), row: 0, column_start: 4, column_end: 13, nodes: vec![Node::Send]})]
    #[case("var", None, LineResult {line: "var.send_test()".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::Send, Node::Send]})]
    #[case("local_var_test", None, LineResult {line: "local_var_test = 2 + 2".to_string(), row: 0, column_start: 0, column_end: 14, nodes: vec![Node::Lvasgn]})]
    #[case("op_assign_test", None, LineResult {line: "op_assign_test += 1".to_string(), row: 0, column_start: 0, column_end: 14, nodes: vec![Node::OpAsgn, Node::Lvasgn]})]
    #[case("or_assign_test", None, LineResult {line: "or_assign_test ||= 1".to_string(), row: 0, column_start: 0, column_end: 14, nodes: vec![Node::OrAsgn, Node::Lvasgn]})]
    #[case("mass_assign_test", None, LineResult {line: "mass_assign_test, test = 1, 2".to_string(), row: 0, column_start: 0, column_end: 16, nodes: vec![Node::Masgn, Node::Mlhs, Node::Lvasgn]})]
    // while
    #[case("while_test",  None, LineResult {line: "while while_test do; test; end".to_string(), row: 0, column_start: 6, column_end: 16, nodes: vec![Node::While, Node::Send]})]
    #[case("while_post_test", None, LineResult {line: "begin while_post_test; end while test".to_string(), row: 0, column_start: 6, column_end: 21, nodes: vec![Node::WhilePost, Node::KwBegin, Node::Send]})]
    // until
    #[case("until_test", None, LineResult {line: "until until_test do; test; end".to_string(), row: 0, column_start: 6, column_end: 16, nodes: vec![Node::Until, Node::Send]})]
    #[case("bar", None, LineResult {line: "until foo do; bar; end".to_string(), row: 0, column_start: 14, column_end: 17, nodes: vec![Node::Until, Node::Send]})]
    #[case("unless_test", None, LineResult {line: "puts 'test' unless unless_test".to_string(), row: 0, column_start: 19, column_end: 30, nodes: vec![Node::IfMod, Node::Send]})]
    #[case("rescue_test", None, LineResult {line: "begin; test; rescue StandardError => rescue_test; true_test; else; else_test; end".to_string(), row: 0, column_start: 37, column_end: 48, nodes: vec![Node::KwBegin, Node::Rescue, Node::RescueBody, Node::Lvasgn]})]
    #[case("bar", None, LineResult {line: "foo(**bar)".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Send, Node::Kwargs, Node::Kwsplat, Node::Send]})]
    #[case("regex_test", None, LineResult {line: "/regex_test/".to_string(), row: 0, column_start: 1, column_end: 11, nodes: vec![Node::Regexp, Node::Str]})]
    #[case("pin_test", None, LineResult {line: "pin_test = 1; case foo; in ^pin_test; end".to_string(), row: 0, column_start: 28, column_end: 36, nodes: vec![Node::Begin, Node::CaseMatch, Node::InPattern, Node::Pin, Node::Lvar]})]
    #[case("global_test", None, LineResult {line: "$global_test = 1000".to_string(), row: 0, column_start: 0, column_end: 12, nodes: vec![Node::Gvasgn]})]
    // const
    #[case("CONST", None, LineResult {line: "CONST = 1".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::Casgn]})]
    #[case("VAR", None, LineResult {line: "VAR::B = 1".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::Casgn]})]
    #[case("1", None, LineResult {line: "VAR::B = 1".to_string(), row: 0, column_start: 9, column_end: 10, nodes: vec![Node::Casgn, Node::Int]})]
    // instance var
    #[case("foo", None, LineResult {line: "@foo".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::Ivar]})]
    #[case("foo", None, LineResult {line: "@foo = 1".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::Ivasgn]})]
    #[case("1", None, LineResult {line: "@foo = 1".to_string(), row: 0, column_start: 7, column_end: 8, nodes: vec![Node::Ivasgn, Node::Int]})]
    // if
    #[case("foo", None, LineResult {line: "if foo; bar; end".to_string(), row: 0, column_start: 3, column_end: 6, nodes: vec![Node::If, Node::Send]})]
    #[case("bar", None, LineResult {line: "if foo; else bar; end".to_string(), row: 0, column_start: 13, column_end: 16, nodes: vec![Node::If, Node::Send]})]
    #[case("if_test", None, LineResult {line: "if if_test...bar_test; end".to_string(), row: 0, column_start: 3, column_end: 10, nodes: vec![Node::If, Node::EFlipFlop, Node::Send]})]
    #[case("test_cond", None, LineResult {line: "test_cond ? test_if_true : test_if_false".to_string(), row: 0, column_start: 0, column_end: 9, nodes: vec![Node::IfTernary, Node::Send]})]
    #[case("foo", None, LineResult {line: "if /foo/; end".to_string(), row: 0, column_start: 4, column_end: 7, nodes: vec![Node::If, Node::MatchCurrentLine, Node::Regexp, Node::Str]})]
    #[case("bar", None, LineResult {line: "case foo; in pattern if bar; end".to_string(), row: 0, column_start: 24, column_end: 27, nodes: vec![Node::CaseMatch, Node::InPattern, Node::IfGuard, Node::Send]})]
    // index
    #[case("foo", None, LineResult {line: "foo[1, row:2, column_start:3]".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::Index, Node::Send]})]
    #[case("foo", None, LineResult {line: "foo[1, row: 2, column_start: 3] = bar".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::IndexAsgn, Node::Send]})]
    // hash
    #[case("hash_test", None, LineResult {line: "test = { hash_test: 42 }".to_string(), row: 0, column_start: 9, column_end: 19, nodes: vec![Node::Lvasgn, Node::Hash, Node::Pair, Node::Sym]})]
    // class var
    #[case("foo", None, LineResult {line: "@@foo".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::Cvar]})]
    #[case("foo", None, LineResult {line: "@@foo = 1".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::Cvasgn]})]
    #[case("1", None, LineResult {line: "@@foo = 1".to_string(), row: 0, column_start: 8, column_end: 9, nodes: vec![Node::Cvasgn, Node::Int]})]
    // global var
    #[case("foo", None, LineResult {line: "$foo".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::Gvar]})]
    #[case("foo", None, LineResult {line: "$foo = 1".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::Gvasgn]})]
    #[case("1", None, LineResult {line: "$foo = 1".to_string(), row: 0, column_start: 7, column_end: 8, nodes: vec![Node::Gvasgn, Node::Int]})]
    // case
    #[case("case_test", None, LineResult {line: "case case_test; when test; end".to_string(), row: 0, column_start: 5, column_end: 14, nodes: vec![Node::Case, Node::Send]})]
    #[case("when_test", None, LineResult {line: "case test; when when_test; end".to_string(), row: 0, column_start: 16, column_end: 25, nodes: vec![Node::Case, Node::When, Node::Send]})]
    #[case("case_in_test", None, LineResult {line: "case foo; in *case_in_test; puts 'v' end".to_string(), row: 0, column_start: 14, column_end: 26, nodes: vec![Node::CaseMatch, Node::InPattern, Node::ArrayPattern, Node::MatchRest, Node::MatchVar]})]
    #[case("else_test", None, LineResult {line: "case 1; when 1; v; else else_test; end".to_string(), row: 0, column_start: 24, column_end: 33, nodes: vec![Node::Case, Node::Send]})]
    #[case("else_match_test", None, LineResult {line: "case 1; in 2; else else_match_test; end".to_string(), row: 0, column_start: 19, column_end: 34, nodes: vec![Node::CaseMatch, Node::Send]})]
    #[case("Foo", None, LineResult {line: "case 1; in Foo(42); end".to_string(), row: 0, column_start: 11, column_end: 14, nodes: vec![Node::CaseMatch, Node::InPattern, Node::ConstPattern]})]
    #[case("bar", None, LineResult {line: "case foo; in [*x, 1 => bar, *y]; end".to_string(), row: 0, column_start: 23, column_end: 26, nodes: vec![Node::CaseMatch, Node::InPattern, Node::FindPattern, Node::MatchAs, Node::MatchVar]})]
    #[case("1", None, LineResult {line: "case foo; in [*x, 1 => bar, *y]; end".to_string(), row: 0, column_start: 18, column_end: 19, nodes: vec![Node::CaseMatch, Node::InPattern, Node::FindPattern, Node::MatchAs, Node::Int]})]
    // proc
    #[case("proc_test", None, LineResult {line: "proc_test = ->(word) { puts word }".to_string(), row: 0, column_start: 0, column_end: 9, nodes: vec![Node::Lvasgn]})]
    #[case("->", None, LineResult {line: "proc_test = ->(word) { puts word }".to_string(), row: 0, column_start: 12, column_end: 14, nodes: vec![Node::Lvasgn, Node::Block, Node::Lambda]})]
    // int
    #[case("10", None, LineResult {line: "int_test = 10".to_string(), row: 0, column_start: 11, column_end: 13, nodes: vec![Node::Lvasgn, Node::Int]})]
    // float
    #[case("1.1", None, LineResult {line: "foo = 1.1".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Lvasgn, Node::Float]})]
    // rational
    #[case("-1r", None, LineResult {line: "rational_test = -1r".to_string(), row: 0, column_start: 16, column_end: 19, nodes: vec![Node::Lvasgn, Node::Rational]})]
    // block pass
    #[case("block_test", None, LineResult {line: "foo(&block_test)".to_string(), row: 0, column_start: 5, column_end: 15, nodes: vec![Node::Send, Node::BlockPass, Node::Send]})]
    // block args
    #[case("foo", None, LineResult {line: "def m(&foo); end".to_string(), row: 0, column_start: 6, column_end: 9, nodes: vec![Node::Def, Node::Arg, Node::Blockarg]})]
    // break
    #[case("break_test", None, LineResult {line: "break :break_test".to_string(), row: 0, column_start: 6, column_end: 17, nodes: vec![Node::Break, Node::Sym]})]
    // csend
    #[case("csend_test", None, LineResult {line: "foo&.csend_test(42)".to_string(), row: 0, column_start: 5, column_end: 15, nodes: vec![Node::CSend]})]
    // super
    #[case("super", None, LineResult {line: "super".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::ZSuper]})]
    // xstr
    #[case("xstr_test", None, LineResult {line: "`sh #{xstr_test}`".to_string(), row: 0, column_start: 6, column_end: 15, nodes: vec![Node::Xstr, Node::Begin, Node::Send]})]
    // yield
    #[case("yield_test", None, LineResult {line: "yield yield_test, row: foo".to_string(), row: 0, column_start: 6, column_end: 16, nodes: vec![Node::Yield, Node::Send]})]
    // true
    #[case("true", None, LineResult {line: "value = true".to_string(), row: 0, column_start: 8, column_end: 12, nodes: vec![Node::Lvasgn, Node::True]})]
    // super
    #[case("super", None, LineResult {line: "super(1, row: 2)".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::Super]})]
    // shadowarg
    #[case("shadow", None, LineResult {line: "proc { |;shadow|}".to_string(), row: 0, column_start: 9, column_end: 15, nodes: vec![Node::Block, Node::Arg, Node::Shadowarg]})]
    // self
    #[case("self", None, LineResult {line: "self.vvvv".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::Send, Node::Self_]})]
    // splat
    #[case("splat", None, LineResult {line: "foo(*splat)".to_string(), row: 0, column_start: 5, column_end: 10, nodes: vec![Node::Send, Node::Splat, Node::Send]})]
    // return
    #[case("ret", None, LineResult {line: "return ret, row: 1".to_string(), row: 0, column_start: 7, column_end: 10, nodes: vec![Node::Return, Node::Send]})]
    // retry
    #[case("retry", None, LineResult {line: "retry if try < vv".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::IfMod, Node::Retry]})]
    // regexp
    #[case("regex", None, LineResult {line: "/regex/mix".to_string(), row: 0, column_start: 1, column_end: 6, nodes: vec![Node::Regexp, Node::Str]})]
    #[case("imx", None, LineResult {line: "/regex/mix".to_string(), row: 0, column_start: 7, column_end: 10, nodes: vec![Node::Regexp, Node::RegOpt]})]
    // redo
    #[case("redo", None, LineResult {line: "redo if test".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::IfMod, Node::Redo]})]
    // proc
    #[case("proc1", None, LineResult {line: "proc { |(proc1, proc2)| }".to_string(), row: 0, column_start: 9, column_end: 14, nodes: vec![Node::Block, Node::Arg, Node::Procarg0, Node::Arg]})]
    // preexe
    #[case("BEGIN", None, LineResult {line: "BEGIN { 1 }".to_string(), row: 0, column_start: 0, column_end: 5, nodes: vec![Node::Preexe]})]
    // postexe
    #[case("END", None, LineResult {line: "END { 1 }".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::Postexe]})]
    // and
    #[case("bar", None, LineResult {line: "foo && bar".to_string(), row: 0, column_start: 7, column_end: 10, nodes: vec![Node::And, Node::Send]})]
    #[case("foo", None, LineResult {line: "foo &&= bar".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::AndAsgn, Node::Lvasgn]})]
    // or
    #[case("bar", None, LineResult {line: "foo || bar".to_string(), row: 0, column_start: 7, column_end: 10, nodes: vec![Node::Or, Node::Send]})]
    // optarg
    #[case("bar", None, LineResult {line: "def foo(bar = 1); end".to_string(), row: 0, column_start: 8, column_end: 11, nodes: vec![Node::Def, Node::Arg, Node::Optarg]})]
    // num block
    #[case("_2", None, LineResult {line: "proc { _2 }".to_string(), row: 0, column_start: 5, column_end: 8, nodes: vec![Node::Numblock]})]
    // nthref
    #[case("$1", None, LineResult {line: "puts \"#$1\"".to_string(), row: 0, column_start: 7, column_end: 8, nodes: vec![Node::Send, Node::Dstr, Node::NthRef]})]
    // nil
    #[case("nil", None, LineResult {line: "v = nil".to_string(), row: 0, column_start: 4, column_end: 7, nodes: vec![Node::Lvasgn, Node::Nil]})]
    // next
    #[case("next", None, LineResult {line: "next 1".to_string(), row: 0, column_start: 0, column_end: 4, nodes: vec![Node::Next]})]
    // backref
    #[case("$+", None, LineResult {line: "$1, $+".to_string(), row: 0, column_start: 4, column_end: 6, nodes: vec![Node::BackRef]})]
    // cbase
    #[case("::", None, LineResult {line: "::X = 10".to_string(), row: 0, column_start: 0, column_end: 2, nodes: vec![Node::Casgn, Node::Cbase]})]
    // complex
    #[case("4i", None, LineResult {line: "3 + 4i".to_string(), row: 0, column_start: 4, column_end: 6, nodes: vec![Node::Send, Node::Complex]})]
    // defined
    #[case("foo", None, LineResult {line: "defined?(foo)".to_string(), row: 0, column_start: 9, column_end: 12, nodes: vec![Node::Defined, Node::Send]})]
    // empty else
    #[case("else", None, LineResult {line: "case foo; in 1; else; end".to_string(), row: 0, column_start: 16, column_end: 20, nodes: vec![Node::CaseMatch, Node::EmptyElse]})]
    // __ENCODING__
    #[case("__ENCODING__", None, LineResult {line: "__ENCODING__".to_string(), row: 0, column_start: 0, column_end: 12, nodes: vec![Node::Encoding]})]
    // __FILE__
    #[case("__FILE__", None, LineResult {line: "__FILE__".to_string(), row: 0, column_start: 0, column_end: 8, nodes: vec![Node::File]})]
    // __LINE__
    #[case("__LINE__", None, LineResult {line: "__LINE__".to_string(), row: 0, column_start: 0, column_end: 8, nodes: vec![Node::Line]})]
    // ensure
    #[case("bar", None, LineResult {line: "begin; foo; ensure; bar; end".to_string(), row: 0, column_start: 20, column_end: 23, nodes: vec![Node::KwBegin, Node::Ensure, Node::Send]})]
    // erange
    #[case("1", None, LineResult {line: "1...3".to_string(), row: 0, column_start: 0, column_end: 1, nodes: vec![Node::Erange, Node::Int]})]
    #[case("2", None, LineResult {line: "2..4".to_string(), row: 0, column_start: 0, column_end: 1, nodes: vec![Node::Irange, Node::Int]})]
    // IFlipFlop
    #[case("foo", None, LineResult {line: "if foo..bar; end".to_string(), row: 0, column_start: 3, column_end: 6, nodes: vec![Node::If, Node::IFlipFlop, Node::Send]})]
    #[case("bar", None, LineResult {line: "if foo..bar; end".to_string(), row: 0, column_start: 8, column_end: 11, nodes: vec![Node::If, Node::IFlipFlop, Node::Send]})]
    // false
    #[case("false", None, LineResult {line: "foo = false".to_string(), row: 0, column_start: 6, column_end: 11, nodes: vec![Node::Lvasgn, Node::False]})]
    // for
    #[case("foo", None, LineResult {line: "for foo in bar; puts 'v'; end".to_string(), row: 0, column_start: 4, column_end: 7, nodes: vec![Node::For, Node::Lvasgn]})]
    // match pattern
    #[case("foo", None, LineResult {line: "foo in pattern".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::MatchPatternP, Node::Send]})]
    #[case("foo", None, LineResult {line: "foo => pattern".to_string(), row: 0, column_start: 0, column_end: 3, nodes: vec![Node::MatchPattern, Node::Send]})]
    #[case("bar", None, LineResult {line: "foo in foo | bar".to_string(), row: 0, column_start: 13, column_end: 16, nodes: vec![Node::MatchPatternP, Node::MatchAlt, Node::MatchVar]})]
    #[case("nil", None, LineResult {line: "foo() in **nil".to_string(), row: 0, column_start: 9, column_end: 14, nodes: vec![Node::MatchPatternP, Node::HashPattern, Node::MatchNilPattern]})]
    // heredoc
    #[case("xhere_test", Some("<<-`HERE`\n  a   #{xhere_test} \nHERE".to_string()), LineResult {line: "  a   #{xhere_test} ".to_string(), row: 1, column_start: 8, column_end: 18, nodes: vec![Node::XHeredoc, Node::Begin, Node::Send]})]
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
}
