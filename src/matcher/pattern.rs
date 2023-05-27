use super::Matcher;
use crate::node::{Node, NodePath, NodeValue};
use anyhow::{anyhow, Result};
use lib_ruby_parser::{Parser, ParserOptions, ParserResult};

pub struct PatternMatcher {
    values: Vec<NodeValue>,
    nodes: Vec<Node>,
}

impl PatternMatcher {
    pub fn new(pattern: String) -> Result<Self> {
        let parser = Parser::new(
            pattern.as_bytes().to_vec(),
            ParserOptions {
                buffer_name: "(eval)".to_string(),
                ..Default::default()
            },
        );
        let ParserResult {
            ast, diagnostics, ..
        } = parser.do_parse();

        if diagnostics.len() > 0 {
            Err(anyhow!(diagnostics
                .iter()
                .map(|d| d.render_message())
                .collect::<Vec<String>>()
                .join("\n")))
        } else {
            let node_values = ast.map(|root| parse(&root, vec![])).unwrap_or(vec![]);
            Ok(PatternMatcher {
                values: node_values.clone(),
                nodes: node_values
                    .into_iter()
                    .map(|n| {
                        let NodeValue(_, node) = n;
                        node
                    })
                    .collect::<Vec<Node>>(),
            })
        }
    }
}

impl Matcher for PatternMatcher {
    fn is_match(&self, node_path: NodePath) -> bool {
        // TODO:
        println!("node_path = {:?}", node_path);
        println!("self ={:?}", &self.nodes);

        if node_path.1.len() > self.nodes.len() {
            node_path.1.ends_with(&self.nodes)
        } else {
            self.nodes.ends_with(&node_path.1)
        }
    }
}

fn parse(node: &lib_ruby_parser::Node, parent: Vec<NodeValue>) -> Vec<NodeValue> {
    match node {
        lib_ruby_parser::Node::Alias(node) => itertools::concat(vec![
            parent,
            parse(&node.to, vec![NodeValue(None, Node::Alias)]),
            parse(&node.from, vec![NodeValue(None, Node::Alias)]),
        ]),

        lib_ruby_parser::Node::And(node) => itertools::concat(vec![
            parent,
            parse(&node.lhs, vec![NodeValue(None, Node::And)]),
            parse(&node.rhs, vec![NodeValue(None, Node::And)]),
        ]),

        lib_ruby_parser::Node::AndAsgn(node) => itertools::concat(vec![
            parent,
            parse(&node.recv, vec![NodeValue(None, Node::AndAsgn)]),
            parse(&node.value, vec![NodeValue(None, Node::AndAsgn)]),
        ]),

        lib_ruby_parser::Node::Arg(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Arg)],
        ]),

        lib_ruby_parser::Node::Args(node) => itertools::concat(vec![
            parent,
            node.args
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Args)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Array(node) => itertools::concat(vec![
            parent,
            node.elements
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::Array)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::ArrayPattern(node) => itertools::concat(vec![
            parent,
            node.elements
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::ArrayPattern)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::ArrayPatternWithTail(node) => itertools::concat(vec![
            parent,
            node.elements
                .iter()
                .flat_map(|statement| {
                    parse(statement, vec![NodeValue(None, Node::ArrayPatternWithTail)])
                })
                .collect(),
        ]),

        lib_ruby_parser::Node::BackRef(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::BackRef)],
        ]),

        lib_ruby_parser::Node::Begin(node) => itertools::concat(vec![
            parent,
            node.statements
                .iter()
                .flat_map(|node| parse(node, vec![NodeValue(None, Node::Begin)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Block(node) => itertools::concat(vec![
            parent,
            parse(&node.call, vec![NodeValue(None, Node::Block)]),
            node.body
                .as_ref()
                .map(|body| parse(&body, vec![NodeValue(None, Node::Block)]))
                .unwrap_or(vec![]),
            node.args
                .as_ref()
                .map(|body| parse(&body, vec![NodeValue(None, Node::Block)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::BlockPass(node) => itertools::concat(vec![
            parent,
            node.value
                .as_ref()
                .map(|v| parse(&v, vec![NodeValue(None, Node::BlockPass)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Blockarg(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(node.name.clone(), Node::Blockarg)],
        ]),

        lib_ruby_parser::Node::Break(node) => itertools::concat(vec![
            parent,
            node.args
                .iter()
                .flat_map(|statement| parse(&statement, vec![NodeValue(None, Node::Break)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::CSend(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.method_name.clone()), Node::CSend)],
            parse(&node.recv, vec![]),
            node.args
                .iter()
                .flat_map(|statement| parse(&statement, vec![]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Case(node) => itertools::concat(vec![
            parent,
            node.expr
                .as_ref()
                .map(|expr| parse(&expr, vec![NodeValue(None, Node::Case)]))
                .unwrap_or(vec![]),
            node.else_body
                .as_ref()
                .map(|else_body| parse(&else_body, vec![NodeValue(None, Node::Case)]))
                .unwrap_or(vec![]),
            node.when_bodies
                .iter()
                .flat_map(|arg| parse(&arg, vec![NodeValue(None, Node::Case)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::CaseMatch(node) => itertools::concat(vec![
            parent,
            parse(&node.expr, vec![NodeValue(None, Node::CaseMatch)]),
            node.else_body
                .as_ref()
                .map(|else_body| parse(else_body, vec![NodeValue(None, Node::CaseMatch)]))
                .unwrap_or(vec![]),
            node.in_bodies
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::CaseMatch)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Casgn(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Casgn)],
            node.scope
                .as_ref()
                .map(|scope| parse(scope, vec![NodeValue(None, Node::Casgn)]))
                .unwrap_or(vec![]),
            node.value
                .as_ref()
                .map(|value| parse(value, vec![NodeValue(None, Node::Casgn)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Cbase(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("::".to_string()), Node::Cbase)],
        ]),

        lib_ruby_parser::Node::Complex(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.value.clone()), Node::Complex)],
        ]),

        lib_ruby_parser::Node::Class(node) => itertools::concat(vec![
            parent,
            parse(&node.name, vec![NodeValue(None, Node::Class)]),
            node.superclass
                .as_ref()
                .map(|superclass| parse(superclass, vec![NodeValue(None, Node::Class)]))
                .unwrap_or(vec![]),
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::Class)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Const(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Const)],
            node.scope
                .as_ref()
                .map(|scope| parse(scope, vec![NodeValue(None, Node::Const)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::ConstPattern(node) => itertools::concat(vec![
            parent,
            parse(&node.const_, vec![NodeValue(None, Node::ConstPattern)]),
            parse(&node.pattern, vec![NodeValue(None, Node::ConstPattern)]),
        ]),

        lib_ruby_parser::Node::Cvar(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Cvar)],
        ]),

        lib_ruby_parser::Node::Cvasgn(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Cvasgn)],
            node.value
                .as_ref()
                .map(|value| parse(value, vec![NodeValue(None, Node::Cvasgn)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Def(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Def)],
            node.args
                .as_ref()
                .map(|args| parse(args, vec![]))
                .unwrap_or(vec![]),
            node.body
                .as_ref()
                .map(|body| parse(body, vec![]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Defined(node) => parse(
            &node.value,
            itertools::concat(vec![parent, vec![NodeValue(None, Node::Defined)]]),
        ),

        lib_ruby_parser::Node::Defs(node) => itertools::concat(vec![
            parent,
            node.body
                .as_ref()
                .map(|body| parse(body, vec![]))
                .unwrap_or(vec![]),
            node.args
                .as_ref()
                .map(|args| parse(args, vec![]))
                .unwrap_or(vec![]),
            vec![NodeValue(Some(node.name.clone()), Node::Defs)],
        ]),

        lib_ruby_parser::Node::Dstr(node) => itertools::concat(vec![
            parent,
            node.parts
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::Dstr)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Dsym(node) => itertools::concat(vec![
            parent,
            node.parts
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::Dsym)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::EFlipFlop(node) => itertools::concat(vec![
            parent,
            node.left
                .as_ref()
                .map(|left| parse(left, vec![NodeValue(None, Node::EFlipFlop)]))
                .unwrap_or(vec![]),
            node.right
                .as_ref()
                .map(|right| parse(right, vec![NodeValue(None, Node::EFlipFlop)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::EmptyElse(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("else".to_string()), Node::EmptyElse)],
        ]),

        lib_ruby_parser::Node::Encoding(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("__ENCODING__".to_string()), Node::Encoding)],
        ]),

        lib_ruby_parser::Node::Ensure(node) => itertools::concat(vec![
            parent,
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::Ensure)]))
                .unwrap_or(vec![]),
            node.ensure
                .as_ref()
                .map(|ensure| parse(ensure, vec![NodeValue(None, Node::Ensure)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Erange(node) => itertools::concat(vec![
            parent,
            node.left
                .as_ref()
                .map(|left| parse(left, vec![NodeValue(None, Node::Erange)]))
                .unwrap_or(vec![]),
            node.right
                .as_ref()
                .map(|right| parse(right, vec![NodeValue(None, Node::Erange)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::False(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("false".to_string()), Node::False)],
        ]),

        lib_ruby_parser::Node::File(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("__FILE__".to_string()), Node::File)],
        ]),

        lib_ruby_parser::Node::FindPattern(node) => node
            .elements
            .iter()
            .flat_map(|statement| {
                parse(
                    statement,
                    itertools::concat(vec![
                        parent.clone(),
                        vec![NodeValue(None, Node::FindPattern)],
                    ]),
                )
            })
            .collect(),

        lib_ruby_parser::Node::Float(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.value.clone()), Node::Float)],
        ]),

        lib_ruby_parser::Node::For(node) => itertools::concat(vec![
            parent,
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::For)]))
                .unwrap_or(vec![]),
            parse(&node.iterator, vec![NodeValue(None, Node::For)]),
            parse(&node.iteratee, vec![NodeValue(None, Node::For)]),
        ]),

        lib_ruby_parser::Node::ForwardArg(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("...".to_string()), Node::ForwardArg)],
        ]),

        lib_ruby_parser::Node::ForwardedArgs(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("...".to_string()), Node::ForwardedArgs)],
        ]),

        lib_ruby_parser::Node::Gvar(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Gvar)],
        ]),

        lib_ruby_parser::Node::Gvasgn(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Gvasgn)],
            node.value
                .as_ref()
                .map(|v| parse(v, vec![NodeValue(None, Node::Gvasgn)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Hash(node) => node
            .pairs
            .iter()
            .flat_map(|statement| {
                parse(
                    statement,
                    itertools::concat(vec![parent.clone(), vec![NodeValue(None, Node::Hash)]]),
                )
            })
            .collect(),

        lib_ruby_parser::Node::HashPattern(node) => node
            .elements
            .iter()
            .flat_map(|statement| {
                parse(
                    statement,
                    itertools::concat(vec![
                        parent.clone(),
                        vec![NodeValue(None, Node::HashPattern)],
                    ]),
                )
            })
            .collect(),

        lib_ruby_parser::Node::Heredoc(node) => node
            .parts
            .iter()
            .flat_map(|statement| {
                parse(
                    statement,
                    itertools::concat(vec![parent.clone(), vec![NodeValue(None, Node::Heredoc)]]),
                )
            })
            .collect(),

        lib_ruby_parser::Node::IFlipFlop(node) => itertools::concat(vec![
            parent,
            node.left
                .as_ref()
                .map(|left| parse(left, vec![NodeValue(None, Node::IFlipFlop)]))
                .unwrap_or(vec![]),
            node.right
                .as_ref()
                .map(|right| parse(right, vec![NodeValue(None, Node::IFlipFlop)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::If(node) => itertools::concat(vec![
            parent,
            node.if_true
                .as_ref()
                .map(|if_true| parse(if_true, vec![NodeValue(None, Node::If)]))
                .unwrap_or(vec![]),
            node.if_false
                .as_ref()
                .map(|if_false| parse(if_false, vec![NodeValue(None, Node::If)]))
                .unwrap_or(vec![]),
            parse(&node.cond, vec![NodeValue(None, Node::If)]),
        ]),

        lib_ruby_parser::Node::IfGuard(node) => parse(
            &node.cond,
            itertools::concat(vec![parent, vec![NodeValue(None, Node::IfGuard)]]),
        ),

        lib_ruby_parser::Node::IfMod(node) => itertools::concat(vec![
            parent,
            node.if_true
                .as_ref()
                .map(|if_true| parse(if_true, vec![NodeValue(None, Node::IfMod)]))
                .unwrap_or(vec![]),
            node.if_false
                .as_ref()
                .map(|if_false| parse(if_false, vec![NodeValue(None, Node::IfMod)]))
                .unwrap_or(vec![]),
            parse(&node.cond, vec![NodeValue(None, Node::IfMod)]),
        ]),

        lib_ruby_parser::Node::IfTernary(node) => itertools::concat(vec![
            parent,
            parse(&node.cond, vec![NodeValue(None, Node::IfTernary)]),
            parse(&node.if_true, vec![NodeValue(None, Node::IfTernary)]),
            parse(&node.if_false, vec![NodeValue(None, Node::IfTernary)]),
        ]),

        lib_ruby_parser::Node::InPattern(node) => itertools::concat(vec![
            parent,
            node.guard
                .as_ref()
                .map(|guard| parse(guard, vec![NodeValue(None, Node::InPattern)]))
                .unwrap_or(vec![]),
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::InPattern)]))
                .unwrap_or(vec![]),
            parse(&node.pattern, vec![NodeValue(None, Node::InPattern)]),
        ]),

        lib_ruby_parser::Node::Index(node) => itertools::concat(vec![
            parent,
            parse(&node.recv, vec![NodeValue(None, Node::Index)]),
            node.indexes
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::Index)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::IndexAsgn(node) => itertools::concat(vec![
            parent,
            node.value
                .as_ref()
                .map(|value| parse(value, vec![NodeValue(None, Node::IndexAsgn)]))
                .unwrap_or(vec![]),
            parse(&node.recv, vec![NodeValue(None, Node::IndexAsgn)]),
            node.indexes
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::IndexAsgn)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Int(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.value.clone()), Node::Int)],
        ]),

        lib_ruby_parser::Node::Irange(node) => itertools::concat(vec![
            parent,
            node.left
                .as_ref()
                .map(|left| parse(left, vec![NodeValue(None, Node::Irange)]))
                .unwrap_or(vec![]),
            node.right
                .as_ref()
                .map(|right| parse(right, vec![NodeValue(None, Node::Irange)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Ivar(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Ivar)],
        ]),

        lib_ruby_parser::Node::Ivasgn(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Ivasgn)],
            node.value
                .as_ref()
                .map(|value| parse(value, vec![NodeValue(None, Node::Ivasgn)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::KwBegin(node) => node
            .statements
            .iter()
            .flat_map(|statement| {
                parse(
                    statement,
                    itertools::concat(vec![parent.clone(), vec![NodeValue(None, Node::KwBegin)]]),
                )
            })
            .collect(),

        lib_ruby_parser::Node::Kwarg(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Kwarg)],
        ]),

        lib_ruby_parser::Node::Kwargs(node) => node
            .pairs
            .iter()
            .flat_map(|statement| {
                parse(
                    statement,
                    itertools::concat(vec![parent.clone(), vec![NodeValue(None, Node::Kwargs)]]),
                )
            })
            .collect(),

        lib_ruby_parser::Node::Kwnilarg(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("**nil".to_string()), Node::Kwnilarg)],
        ]),

        lib_ruby_parser::Node::Kwoptarg(node) => itertools::concat(vec![
            parent,
            parse(&node.default, vec![NodeValue(None, Node::Kwoptarg)]),
            vec![NodeValue(Some(node.name.clone()), Node::Kwnilarg)],
        ]),

        lib_ruby_parser::Node::Kwrestarg(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(node.name.clone(), Node::Kwrestarg)],
        ]),

        lib_ruby_parser::Node::Kwsplat(node) => parse(
            &node.value,
            itertools::concat(vec![parent, vec![NodeValue(None, Node::Kwsplat)]]),
        ),

        lib_ruby_parser::Node::Lambda(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("->".to_string()), Node::Lambda)],
        ]),

        lib_ruby_parser::Node::Line(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("__LINE__".to_string()), Node::Line)],
        ]),

        lib_ruby_parser::Node::Lvar(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Lvar)],
        ]),

        lib_ruby_parser::Node::Lvasgn(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Lvasgn)],
            node.value
                .as_ref()
                .map(|value| parse(&value, vec![]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Masgn(node) => itertools::concat(vec![
            parent,
            parse(&node.lhs, vec![NodeValue(None, Node::Masgn)]),
            parse(&node.rhs, vec![NodeValue(None, Node::Masgn)]),
        ]),

        lib_ruby_parser::Node::MatchAlt(node) => itertools::concat(vec![
            parent,
            parse(&node.lhs, vec![NodeValue(None, Node::MatchAlt)]),
            parse(&node.rhs, vec![NodeValue(None, Node::MatchAlt)]),
        ]),

        lib_ruby_parser::Node::MatchAs(node) => itertools::concat(vec![
            parent,
            parse(&node.value, vec![NodeValue(None, Node::MatchAs)]),
            parse(&node.as_, vec![NodeValue(None, Node::MatchAs)]),
        ]),

        lib_ruby_parser::Node::MatchCurrentLine(node) => parse(
            &node.re,
            itertools::concat(vec![parent, vec![NodeValue(None, Node::MatchCurrentLine)]]),
        ),

        lib_ruby_parser::Node::MatchNilPattern(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("**nil".to_string()), Node::MatchNilPattern)],
        ]),

        lib_ruby_parser::Node::MatchPattern(node) => itertools::concat(vec![
            parent,
            parse(&node.value, vec![NodeValue(None, Node::MatchPattern)]),
            parse(&node.pattern, vec![NodeValue(None, Node::MatchPattern)]),
        ]),

        lib_ruby_parser::Node::MatchPatternP(node) => itertools::concat(vec![
            parent,
            parse(&node.value, vec![NodeValue(None, Node::MatchPatternP)]),
            parse(&node.pattern, vec![NodeValue(None, Node::MatchPatternP)]),
        ]),

        lib_ruby_parser::Node::MatchRest(node) => node
            .name
            .as_ref()
            .map(|name| parse(name, vec![NodeValue(None, Node::MatchRest)]))
            .unwrap_or(vec![]),

        lib_ruby_parser::Node::MatchVar(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::MatchVar)],
        ]),

        lib_ruby_parser::Node::MatchWithLvasgn(node) => itertools::concat(vec![
            parent,
            parse(&node.re, vec![NodeValue(None, Node::MatchWithLvasgn)]),
            parse(&node.value, vec![NodeValue(None, Node::MatchWithLvasgn)]),
        ]),

        lib_ruby_parser::Node::Mlhs(node) => itertools::concat(vec![
            parent,
            node.items
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Mlhs)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Module(node) => itertools::concat(vec![
            parent,
            parse(&node.name, vec![NodeValue(None, Node::Module)]),
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::Module)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Next(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("next".to_string()), Node::Next)],
            node.args
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Next)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Nil(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("nil".to_string()), Node::Nil)],
        ]),

        lib_ruby_parser::Node::NthRef(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(format!("${}", &node.name)), Node::NthRef)],
        ]),

        lib_ruby_parser::Node::Numblock(node) => itertools::concat(vec![
            parent,
            parse(&node.call, vec![NodeValue(None, Node::Numblock)]),
            parse(&node.body, vec![NodeValue(None, Node::Numblock)]),
            vec![NodeValue(
                Some(format!("_{}", node.numargs)),
                Node::Numblock,
            )],
        ]),

        lib_ruby_parser::Node::OpAsgn(node) => itertools::concat(vec![
            parent,
            parse(&node.recv, vec![NodeValue(None, Node::OpAsgn)]),
            parse(&node.value, vec![NodeValue(None, Node::OpAsgn)]),
        ]),

        lib_ruby_parser::Node::Optarg(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Optarg)],
        ]),

        lib_ruby_parser::Node::Or(node) => itertools::concat(vec![
            parent,
            parse(&node.lhs, vec![NodeValue(None, Node::Or)]),
            parse(&node.rhs, vec![NodeValue(None, Node::Or)]),
        ]),

        lib_ruby_parser::Node::OrAsgn(node) => itertools::concat(vec![
            parent,
            parse(&node.recv, vec![NodeValue(None, Node::OrAsgn)]),
            parse(&node.value, vec![NodeValue(None, Node::OrAsgn)]),
        ]),

        lib_ruby_parser::Node::Pair(node) => itertools::concat(vec![
            parent,
            parse(&node.key, vec![NodeValue(None, Node::Pair)]),
            parse(&node.value, vec![NodeValue(None, Node::Pair)]),
        ]),

        lib_ruby_parser::Node::Pin(node) => parse(
            &node.var,
            itertools::concat(vec![parent, vec![NodeValue(None, Node::Pin)]]),
        ),

        lib_ruby_parser::Node::Postexe(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("END".to_string()), Node::Postexe)],
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::Postexe)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Preexe(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("BEGIN".to_string()), Node::Preexe)],
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::Preexe)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Procarg0(node) => node
            .args
            .iter()
            .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Procarg0)]))
            .collect(),

        lib_ruby_parser::Node::Rational(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.value.clone()), Node::Rational)],
        ]),

        lib_ruby_parser::Node::Redo(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("redo".to_string()), Node::Redo)],
        ]),

        lib_ruby_parser::Node::RegOpt(node) => node
            .options
            .as_ref()
            .map(|options| {
                itertools::concat(vec![
                    parent,
                    vec![NodeValue(Some(options.clone()), Node::RegOpt)],
                ])
            })
            .unwrap_or(vec![]),

        lib_ruby_parser::Node::Regexp(node) => itertools::concat(vec![
            parent,
            node.options
                .as_ref()
                .map(|options| {
                    itertools::concat(vec![
                        parse(options, vec![NodeValue(None, Node::Regexp)]),
                        node.parts
                            .iter()
                            .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Regexp)]))
                            .collect(),
                    ])
                })
                .unwrap_or(
                    node.parts
                        .iter()
                        .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Regexp)]))
                        .collect(),
                ),
        ]),

        lib_ruby_parser::Node::Rescue(node) => itertools::concat(vec![
            parent,
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::Rescue)]))
                .unwrap_or(vec![]),
            node.else_
                .as_ref()
                .map(|else_| parse(else_, vec![NodeValue(None, Node::Rescue)]))
                .unwrap_or(vec![]),
            node.rescue_bodies
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::Rescue)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::RescueBody(node) => itertools::concat(vec![
            parent,
            node.exc_list
                .as_ref()
                .map(|exc_list| parse(exc_list, vec![NodeValue(None, Node::RescueBody)]))
                .unwrap_or(vec![]),
            node.exc_var
                .as_ref()
                .map(|exc_var| parse(exc_var, vec![NodeValue(None, Node::RescueBody)]))
                .unwrap_or(vec![]),
            node.body
                .as_ref()
                .map(|body| parse(body, vec![NodeValue(None, Node::RescueBody)]))
                .unwrap_or(vec![]),
        ]),

        lib_ruby_parser::Node::Restarg(node) => node
            .name
            .as_ref()
            .map(|s| {
                itertools::concat(vec![
                    parent,
                    vec![NodeValue(Some(s.to_string()), Node::Restarg)],
                ])
            })
            .unwrap_or(vec![]),

        lib_ruby_parser::Node::Retry(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("retry".to_string()), Node::Retry)],
        ]),

        lib_ruby_parser::Node::Return(node) => itertools::concat(vec![
            parent,
            node.args
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Return)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::SClass(node) => itertools::concat(vec![
            parent,
            node.body
                .as_ref()
                .map(|body| {
                    itertools::concat(vec![
                        parse(&node.expr, vec![NodeValue(None, Node::SClass)]),
                        parse(body, vec![NodeValue(None, Node::SClass)]),
                    ])
                })
                .unwrap_or(parse(&node.expr, vec![NodeValue(None, Node::SClass)])),
        ]),

        lib_ruby_parser::Node::Splat(node) => node
            .value
            .as_ref()
            .map(|value| {
                parse(
                    value,
                    itertools::concat(vec![parent, vec![NodeValue(None, Node::Splat)]]),
                )
            })
            .unwrap_or(vec![]),

        lib_ruby_parser::Node::Self_(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("self".to_string()), Node::Self_)],
        ]),

        lib_ruby_parser::Node::Send(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.method_name.clone()), Node::Send)],
            node.recv
                .as_ref()
                .map(|node| parse(node, vec![]))
                .unwrap_or(vec![]),
            node.args
                .iter()
                .flat_map(|statement| parse(statement, vec![]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Shadowarg(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some(node.name.clone()), Node::Shadowarg)],
        ]),

        lib_ruby_parser::Node::Str(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(
                Some(node.value.to_string().unwrap_or("".to_string())),
                Node::Str,
            )],
        ]),

        lib_ruby_parser::Node::Super(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("super".to_string()), Node::Super)],
            node.args
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Args)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Sym(node) => itertools::concat(vec![
            parent,
            vec![NodeValue(
                Some(node.name.to_string().unwrap_or("".to_string())),
                Node::Sym,
            )],
        ]),

        lib_ruby_parser::Node::True(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("true".to_string()), Node::True)],
        ]),

        lib_ruby_parser::Node::Undef(node) => itertools::concat(vec![
            parent,
            node.names
                .iter()
                .flat_map(|statement| parse(statement, vec![NodeValue(None, Node::Undef)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::UnlessGuard(node) => parse(
            &node.cond,
            itertools::concat(vec![parent, vec![NodeValue(None, Node::UnlessGuard)]]),
        ),

        lib_ruby_parser::Node::Until(node) => match &node.body {
            Some(body) => itertools::concat(vec![
                parent,
                parse(&node.cond, vec![NodeValue(None, Node::Until)]),
                parse(body, vec![NodeValue(None, Node::Until)]),
            ]),
            _ => parse(
                &node.cond,
                itertools::concat(vec![parent, vec![NodeValue(None, Node::Until)]]),
            ),
        },

        lib_ruby_parser::Node::UntilPost(node) => itertools::concat(vec![
            parent,
            parse(&node.cond, vec![NodeValue(None, Node::UntilPost)]),
            parse(&node.body, vec![NodeValue(None, Node::UntilPost)]),
        ]),

        lib_ruby_parser::Node::When(node) => match &node.body {
            Some(body) => itertools::concat(vec![
                parent,
                node.patterns
                    .iter()
                    .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::When)]))
                    .collect(),
                parse(body, vec![NodeValue(None, Node::When)]),
            ]),
            _ => itertools::concat(vec![
                parent,
                node.patterns
                    .iter()
                    .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::When)]))
                    .collect(),
            ]),
        },

        lib_ruby_parser::Node::While(node) => itertools::concat(vec![
            parent,
            node.body
                .as_ref()
                .map(|body| {
                    itertools::concat(vec![
                        parse(&node.cond, vec![NodeValue(None, Node::While)]),
                        parse(body, vec![NodeValue(None, Node::While)]),
                    ])
                })
                .unwrap_or(parse(&node.cond, vec![NodeValue(None, Node::While)])),
        ]),

        lib_ruby_parser::Node::WhilePost(node) => itertools::concat(vec![
            parent,
            parse(&node.cond, vec![NodeValue(None, Node::WhilePost)]),
            parse(&node.body, vec![NodeValue(None, Node::WhilePost)]),
        ]),

        lib_ruby_parser::Node::XHeredoc(node) => itertools::concat(vec![
            parent,
            node.parts
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::XHeredoc)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Xstr(node) => itertools::concat(vec![
            parent,
            node.parts
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Xstr)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::Yield(node) => itertools::concat(vec![
            parent,
            node.args
                .iter()
                .flat_map(|arg| parse(arg, vec![NodeValue(None, Node::Yield)]))
                .collect(),
        ]),

        lib_ruby_parser::Node::ZSuper(_node) => itertools::concat(vec![
            parent,
            vec![NodeValue(Some("super".to_string()), Node::ZSuper)],
        ]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("test.method", vec![NodeValue(Some("method".to_string()), Node::Send),
                               NodeValue(Some("test".to_string()), Node::Send)])]
    #[case("foo && bar", vec![NodeValue(None, Node::And),
                              NodeValue(Some("foo".to_string()), Node::Send),
                              NodeValue(None, Node::And),
                              NodeValue(Some("bar".to_string()), Node::Send)])]
    #[case("foo &&= bar", vec![NodeValue(None, Node::AndAsgn),
                               NodeValue(Some("foo".to_string()), Node::Lvasgn),
                               NodeValue(None, Node::AndAsgn),
                               NodeValue(Some("bar".to_string()), Node::Send)])]
    #[case("[1, 'str', 1.0]", vec![NodeValue(None, Node::Array),
                                   NodeValue(Some("1".to_string()), Node::Int),
                                   NodeValue(None, Node::Array),
                                   NodeValue(Some("str".to_string()), Node::Str),
                                   NodeValue(None, Node::Array),
                                   NodeValue(Some("1.0".to_string()), Node::Float)])]
    #[case("break :foo", vec![NodeValue(None, Node::Break), NodeValue(Some("foo".to_string()), Node::Sym)])]
    #[case("CONST = 1", vec![NodeValue(Some("CONST".to_string()), Node::Casgn), NodeValue(None, Node::Casgn), NodeValue(Some("1".to_string()), Node::Int)])]
    #[case("foo&.bar(42)", vec![NodeValue(Some("bar".to_string()), Node::CSend),
                                NodeValue(Some("foo".to_string()), Node::Send),
                                NodeValue(Some("42".to_string()), Node::Int)])]
    #[case("def test(vvvv)
              puts 'vvv'
            end", vec![NodeValue(Some("test".to_string()), Node::Def),
                       NodeValue(None, Node::Args),
                       NodeValue(Some("vvvv".to_string()), Node::Arg),
                       NodeValue(Some("puts".to_string()), Node::Send),
                       NodeValue(Some("vvv".to_string()), Node::Str)])]
    #[case("foo = 1", vec![NodeValue(Some("foo".to_string()), Node::Lvasgn),
                           NodeValue(Some("1".to_string()), Node::Int)])]
    #[case("alias :foo :var", vec![NodeValue(None, Node::Alias),
                                   NodeValue(Some("foo".to_string()), Node::Sym),
                                   NodeValue(None, Node::Alias),
                                   NodeValue(Some("var".to_string()), Node::Sym)])]
    #[case("yield foo, row: bar", vec![NodeValue(None, Node::Yield),
                                       NodeValue(Some("foo".to_string()), Node::Send),
                                       NodeValue(None, Node::Yield),
                                       NodeValue(None, Node::Kwargs),
                                       NodeValue(None, Node::Pair),
                                       NodeValue(Some("row".to_string()), Node::Sym),
                                       NodeValue(None, Node::Pair),
                                       NodeValue(Some("bar".to_string()), Node::Send)])]
    fn parse_pattern(#[case] pattern: String, #[case] expected: Vec<NodeValue>) {
        let matcher = PatternMatcher::new(pattern).unwrap();

        // TODO:
        println!("{:?}", matcher.values);

        assert_eq!(matcher.values, expected);
    }
}
