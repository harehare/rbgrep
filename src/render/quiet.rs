use std::io;

use anyhow::Result;

use crate::render::Render;
use crate::source::FileResult;

pub struct QuietRender {}

impl Render for QuietRender {
    fn render(&self, _w: &mut dyn io::Write, _result: &FileResult) -> Result<()> {
        Ok(())
    }
}
