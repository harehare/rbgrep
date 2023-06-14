use std::sync::Arc;

use anyhow::Result;
use regex::Regex;

use super::Matcher;

pub struct RegexMatcher {
    re: Regex,
}

impl RegexMatcher {
    pub fn new_matcher(regex: &str) -> Result<Arc<dyn Matcher>> {
        let re = Regex::new(regex)?;
        Ok(Arc::new(RegexMatcher { re }))
    }
}

impl Matcher for RegexMatcher {
    fn is_match(&self, text: String) -> bool {
        self.re.is_match(text.as_str())
    }
}

#[cfg(test)]
mod tests {
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
            RegexMatcher::new_matcher(regex.as_str())
                .unwrap()
                .is_match(text)
        )
    }
}
