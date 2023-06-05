use clap::ValueEnum;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone)]
pub struct NodePath(pub String, pub Nodes);

#[derive(Debug, PartialEq, Clone)]
pub struct NodeValue(pub Option<String>, pub Node);

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
        let (target, part) = if self.0.len() > nodes.0.len() {
            (&self.0, &nodes.0)
        } else {
            (&nodes.0, &self.0)
        };

        for w in target.windows(part.len()) {
            if w == part {
                return true;
            }
        }

        false
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
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
