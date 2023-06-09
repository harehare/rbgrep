mod regex;

pub use self::regex::RegexMatcher;

mod text;
pub use self::text::TextMatcher;

pub trait Matcher: Send + Sync + 'static {
    fn is_match(&self, text: String) -> bool;
}
