use std::io;

use anyhow::Result;

use crate::source::FileResult;

mod text;
pub use self::text::TextRender;

mod count;
pub use self::count::CountRender;

mod quiet;
pub use self::quiet::QuietRender;

mod json;
pub use self::json::JsonRender;

pub trait Render {
    fn render<W: io::Write>(&self, w: &mut W, result: &FileResult) -> Result<()>;
}
