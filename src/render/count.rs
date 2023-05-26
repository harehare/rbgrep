use std::io;

use anyhow::{anyhow, Result};
use colored::*;

use crate::render::Render;
use crate::source::FileResult;

pub struct CountRender {
    pub with_filename: bool,
}

impl Render for CountRender {
    fn render<W: io::Write>(&self, w: &mut W, result: &FileResult) -> Result<()> {
        (if self.with_filename {
            w.write_all(
                format!("{}:{}\n", result.filename.magenta(), result.results.len()).as_bytes(),
            )
        } else {
            w.write_all(format!("{}\n", result.results.len()).as_bytes())
        })
        .map_err(|_| anyhow!("Failed write"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{node::Node, source::LineResult};
    use rstest::rstest;

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes: vec![Node::Class]}],
        false,
        "1\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes: vec![Node::Class]}],
        true,
        "file:1\n".to_string(),
    )]
    fn test_render_count(
        #[case] lines: Vec<String>,
        #[case] results: Vec<LineResult>,
        #[case] with_filename: bool,
        #[case] expected: String,
    ) {
        let count_render = CountRender { with_filename };

        let result = FileResult {
            filename: "file".to_string(),
            lines,
            results,
        };
        let mut out: Vec<u8> = vec![];
        count_render.render::<Vec<u8>>(&mut out, &result).unwrap();

        assert_eq!(String::from_utf8(out).unwrap(), expected);
    }
}
