use std::io;

use anyhow::Result;

use crate::source::FileResult;

mod text;
pub use self::text::TextRender;

mod count;
pub use self::count::CountRender;

pub trait Render {
    fn render<W: io::Write>(&self, w: &mut W, result: &FileResult) -> Result<()>;
}
