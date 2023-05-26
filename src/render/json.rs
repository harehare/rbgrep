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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{node::Node, source::LineResult};
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
        vec![LineResult {line:"class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes:vec![Node::Class]}],
        "{\"fileName\":\"file\",\"results\":[{\"line\":\"class Test\",\"row\":0,\"nodes\":[\"Class\"],\"columnStart\":6,\"columnEnd\":8}]}".to_string(),
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line:"class Test".to_string(), row: 0, column_start: 6, column_end: 8, nodes: vec![Node::Class]}],
        "{\"fileName\":\"file\",\"results\":[{\"line\":\"class Test\",\"row\":0,\"nodes\":[\"Class\"],\"columnStart\":6,\"columnEnd\":8}]}".to_string(),
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
        json_render.render::<Vec<u8>>(&mut out, &result).unwrap();

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
        assert_eq!(
            json_render.render::<ErrorWrite>(&mut out, &result).is_err(),
            true
        );
    }
}
