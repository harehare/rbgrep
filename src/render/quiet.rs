use crate::render::Render;
use crate::source::FileResult;
use std::io;

pub struct QuietRender {}

impl Render for QuietRender {
    fn render(&self, _w: &mut dyn io::Write, _result: &FileResult) -> Result<(), io::Error> {
        Ok(())
    }
}
