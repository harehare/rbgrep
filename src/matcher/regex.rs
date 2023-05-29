use anyhow::Result;
use regex::Regex;

use crate::node::NodePath;

use super::Matcher;

pub struct RegexMatcher {
    re: Regex,
}

impl RegexMatcher {
    pub fn new(regex: &str) -> Result<Self> {
        let re = Regex::new(regex)?;
        Ok(RegexMatcher { re })
    }
}

impl Matcher for RegexMatcher {
    fn is_match(&self, node_path: NodePath) -> bool {
        let NodePath(text, _) = node_path;
        self.re.is_match(text.as_str())
    }
}

#[cfg(test)]
mod tests {
    use crate::node::{Node, NodePath, Nodes};

    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("te.+", "test_string", true)]
    #[case("te.+", "tst_string", false)]
    #[should_panic]
    #[case("++", "TST_STRING", false)]
    fn is_regex(#[case] regex: String, #[case] text: String, #[case] expected: bool) {
        assert_eq!(
            expected,
            RegexMatcher::new(regex.as_str())
                .unwrap()
                .is_match(NodePath(text, Nodes::new(vec![Node::Begin])))
        )
    }
}
