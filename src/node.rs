use clap::ValueEnum;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Nodes(Vec<Node>);

impl Nodes {
    pub fn new(nodes: Vec<Node>) -> Nodes {
        Nodes(nodes)
    }

    pub fn empty() -> Nodes {
        Nodes(vec![])
    }

    pub fn append(&self, node: Node) -> Nodes {
        Nodes::new(itertools::concat(vec![self.0.clone(), vec![node]]))
    }

    pub fn merge(&self, nodes: Nodes) -> Nodes {
        Nodes::new(itertools::concat(vec![self.0.clone(), nodes.to_vec()]))
    }

    pub fn to_vec(&self) -> Vec<Node> {
        self.0.clone()
    }

    pub fn contains(&self, nodes: &Nodes) -> bool {
        if nodes.0.is_empty() {
            return true;
        }

        if self.0.len() < nodes.0.len() {
            return false;
        }

        for w in self.0.windows(nodes.0.len()) {
            if w == nodes.0 {
                return true;
            }
        }

        false
    }
}

#[derive(Debug, PartialEq, Clone, ValueEnum, strum_macros::Display, Serialize)]
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

#[cfg(test)]
mod tests {
    use crate::node::{Node, Nodes};
    use rstest::rstest;

    #[rstest]
    #[case(Nodes::empty(), Nodes::empty(), true)]
    #[case(Nodes::new(vec![Node::Class, Node::Def, Node::Args]), Nodes::new(vec![Node::Class, Node::Def, Node::Args]), true)]
    #[case(Nodes::new(vec![Node::Class, Node::Def, Node::Args]), Nodes::new(vec![Node::Def, Node::Args]), true)]
    #[case(Nodes::new(vec![Node::Def, Node::Args]), Nodes::new(vec![Node::Class, Node::Def, Node::Args]), false)]
    #[case(Nodes::new(vec![Node::Def, Node::Args]), Nodes::new(vec![Node::Class, Node::Args]), false)]
    fn test_contains(#[case] target: Nodes, #[case] part: Nodes, #[case] expected: bool) {
        assert_eq!(target.contains(&part), expected);
    }
}
