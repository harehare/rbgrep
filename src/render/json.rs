use std::io;

use anyhow::{anyhow, Result};

use crate::render::Render;
use crate::source::FileResult;

pub struct JsonRender {}

impl Render for JsonRender {
    fn render<W: io::Write>(&self, w: &mut W, result: &FileResult) -> Result<()> {
        w.write_all(serde_json::to_string(&result)?.as_bytes())
            .map_err(|_| anyhow!("write failed"))
    }
}
