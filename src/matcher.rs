mod regex;
use crate::node::NodePath;

pub use self::regex::RegexMatcher;

mod text;
pub use self::text::TextMatcher;

pub trait Matcher {
    fn is_match(&self, node_path: NodePath) -> bool;
}
