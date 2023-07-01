use crate::render::Render;
use crate::source::FileResult;
use std::io;

pub struct CsvRender {
    pub delimiter: Option<String>,
}

impl Render for CsvRender {
    fn render(&self, w: &mut dyn io::Write, result: &FileResult) -> Result<(), io::Error> {
        let delimiter = self.delimiter.clone().unwrap_or(",".to_string());
        let lines = result
            .results
            .iter()
            .map(|r| {
                format!(
                    "{}{}{}{}{}{}{}{}[{}]\n",
                    r.start.row,
                    delimiter,
                    r.start.column,
                    delimiter,
                    result.filename,
                    delimiter,
                    r.line,
                    delimiter,
                    r.nodes
                        .to_vec()
                        .iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<String>>()
                        .join(",")
                )
            })
            .collect::<Vec<String>>()
            .join("");

        w.write_all(lines.as_bytes())
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
        "0,6,file,class Test,[Class]\n"
    )]
    #[case(
        vec!["class Test".to_string()],
        vec![LineResult {line:"class Test".to_string(), start: Position{row:0, column: 6}, end: Position{row:0, column: 8}, nodes: Nodes::new(vec![Node::Class])}],
        "0,6,file,class Test,[Class]\n"
    )]
    fn test_render_csv(
        #[case] lines: Vec<String>,
        #[case] results: Vec<LineResult>,
        #[case] expected: String,
    ) {
        let csv_render = CsvRender {
            delimiter: Some(",".to_string()),
        };

        let result = FileResult {
            filename: "file".to_string(),
            lines,
            results,
        };
        let mut out: Vec<u8> = vec![];
        csv_render.render(&mut out, &result).unwrap();
        assert_eq!(String::from_utf8(out).unwrap(), expected);
    }
}
