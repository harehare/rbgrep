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
    use crate::source::{LineResult, Node};
    use rstest::rstest;

    fn line_result(
        line: String,
        row: usize,
        column_start: usize,
        column_end: usize,
        nodes: Vec<Node>,
    ) -> LineResult {
        LineResult {
            line,
            row,
            nodes,
            column_start,
            column_end,
        }
    }

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
        false,
        "1\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![line_result("class Test".to_string(), 0, 6, 8, vec![Node::Class])],
        true,
        "file:1\n".to_string(),
    )]
    fn test_print_count(
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
