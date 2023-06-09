use crate::render::Render;
use crate::source::FileResult;
use colored::*;
use std::io;

pub struct CountRender {
    pub with_filename: bool,
}

impl Render for CountRender {
    fn render(&self, w: &mut dyn io::Write, result: &FileResult) -> Result<(), io::Error> {
        if self.with_filename {
            w.write_all(
                format!("{}:{}\n", result.filename.magenta(), result.results.len()).as_bytes(),
            )
        } else {
            w.write_all(format!("{}\n", result.results.len()).as_bytes())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        node::{Node, Nodes},
        source::{LineResult, Position},
    };
    use rstest::rstest;

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), start: Position{row:0, column: 6}, end: Position{row:0, column: 8}, nodes: Nodes::new(vec![Node::Class])}],
        false,
        "1\n".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line: "class Test".to_string(), start: Position{row:0, column: 6}, end: Position{row:0, column: 8}, nodes: Nodes::new(vec![Node::Class])}],
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
        count_render.render(&mut out, &result).unwrap();

        assert_eq!(String::from_utf8(out).unwrap(), expected);
    }
}
