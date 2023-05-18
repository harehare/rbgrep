use std::io;

use anyhow::Result;

use crate::render::Render;
use crate::source::FileResult;

pub struct QuietRender {}

impl Render for QuietRender {
    fn render<W: io::Write>(&self, _w: &mut W, _result: &FileResult) -> Result<()> {
        Ok(())
    }
}
