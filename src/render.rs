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

mod csv;
pub use self::csv::CsvRender;

pub trait Render: Send + Sync + 'static {
    fn render(&self, w: &mut dyn io::Write, result: &FileResult) -> Result<()>;
}
