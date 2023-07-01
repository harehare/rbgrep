use crate::render::Render;
use crate::source::FileResult;
use std::io;

pub struct JsonRender {}

impl Render for JsonRender {
    fn render(&self, w: &mut dyn io::Write, result: &FileResult) -> Result<(), io::Error> {
        w.write_all(serde_json::to_string(&result)?.as_bytes())
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

    struct ErrorWrite {}

    impl io::Write for ErrorWrite {
        fn write(&mut self, _buf: &[u8]) -> io::Result<usize> {
            Err(io::Error::new(io::ErrorKind::Other, "error"))
        }

        fn flush(&mut self) -> io::Result<()> {
            Ok(())
        }
    }

    #[rstest]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line:"class Test".to_string(), start: Position{row:0, column: 6}, end: Position{row:0, column: 8}, nodes:Nodes::new(vec![Node::Class])}],
        "{\"fileName\":\"file\",\"results\":[{\"line\":\"class Test\",\"nodes\":[\"Class\"],\"start\":{\"row\":0,\"column\":6},\"end\":{\"row\":0,\"column\":8}}]}"
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line:"class Test".to_string(), start: Position{row:0, column: 6}, end: Position{row:0, column: 8}, nodes: Nodes::new(vec![Node::Class])}],
        "{\"fileName\":\"file\",\"results\":[{\"line\":\"class Test\",\"nodes\":[\"Class\"],\"start\":{\"row\":0,\"column\":6},\"end\":{\"row\":0,\"column\":8}}]}"
    )]
    fn test_render_json(
        #[case] lines: Vec<String>,
        #[case] results: Vec<LineResult>,
        #[case] expected: String,
    ) {
        let json_render = JsonRender {};

        let result = FileResult {
            filename: "file".to_string(),
            lines,
            results,
        };
        let mut out: Vec<u8> = vec![];
        json_render.render(&mut out, &result).unwrap();

        assert_eq!(String::from_utf8(out).unwrap(), expected);
    }

    #[test]
    fn test_render_error() {
        let json_render = JsonRender {};

        let result = FileResult {
            filename: "file".to_string(),
            lines: vec![],
            results: vec![],
        };
        let mut out = ErrorWrite {};
        assert_eq!(json_render.render(&mut out, &result).is_err(), true);
    }
}
