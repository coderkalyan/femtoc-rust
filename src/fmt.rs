pub mod syntax_errors;

use std::io::{Write, Result};

pub struct IndentingWriter<'a, const W: usize> {
    depth: u32,
    writer: &'a mut dyn Write,
    needs_indent: bool,
}

impl <'a, const W: usize> IndentingWriter<'a, W> {
    pub fn new(writer: &'a mut dyn Write) -> IndentingWriter<'a, W> {
        IndentingWriter { depth: 0, writer, needs_indent: false }
    }

    pub fn newline(&mut self) -> Result<()> {
        if self.needs_indent {
            self.write_indent()?;
        }
        self.writer.write_all("\n".as_bytes())?;
        self.needs_indent = true;

        Ok(())
    }

    pub fn indent(&mut self) {
        self.depth += 1;
    }

    pub fn dedent(&mut self) {
        assert!(self.depth >= 1);
        self.depth -= 1;
    }

    fn write_indent(&mut self) -> Result<()> {
        self.needs_indent = false;

        let mut i: u32 = 0;
        while i < self.depth {
            self.writer.write_all(" ".repeat(W).as_bytes())?;
            i += 1;
        }

        Ok(())
    }
}
