use anyhow::Result;
use regex::Regex;

pub trait Matcher {
    fn is_match(&self, text: &str) -> bool;
}

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
    fn is_match(&self, text: &str) -> bool {
        self.re.is_match(text)
    }
}

pub struct TextMatcher {
    query: String,
    exact_match: bool,
    case_sensitive: bool,
}

impl TextMatcher {
    pub fn new(query: String, exact_match: bool, case_sensitive: bool) -> Self {
        TextMatcher {
            query,
            exact_match,
            case_sensitive,
        }
    }
}

impl Matcher for TextMatcher {
    fn is_match(&self, text: &str) -> bool {
        match (self.exact_match, self.case_sensitive) {
            (false, true) => text.contains(self.query.clone().as_str()),
            (false, false) => text
                .to_lowercase()
                .contains(self.query.clone().to_lowercase().as_str()),
            (true, true) => *text == self.query,
            (true, false) => text.to_string().to_lowercase() == self.query.to_lowercase(),
        }
    }
}

#[cfg(test)]
mod tests {

    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case("test", false, false, "TEST_STRING", true)]
    #[case("test", true, true, "test", true)]
    #[case("test", true, false, "TEST_STRING", false)]
    #[case("test", true, false, "test", true)]
    #[case("test", true, false, "TEST_STRING", false)]
    #[case("test", false, true, "test_string", true)]
    #[case("test", false, true, "TEST_STRING", false)]
    fn text_match(
        #[case] query: String,
        #[case] exact_match: bool,
        #[case] case_sensitive: bool,
        #[case] text: String,
        #[case] expected: bool,
    ) {
        assert_eq!(
            expected,
            TextMatcher::new(query, exact_match, case_sensitive).is_match(text.as_str())
        )
    }

    #[rstest]
    #[case("te.+", "test_string", true)]
    #[case("te.+", "tst_string", false)]
    #[should_panic]
    #[case("++", "TST_STRING", false)]
    fn regex_match(#[case] regex: String, #[case] text: String, #[case] expected: bool) {
        assert_eq!(
            expected,
            RegexMatcher::new(regex.as_str())
                .unwrap()
                .is_match(text.as_str())
        )
    }
}
