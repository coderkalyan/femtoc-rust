use std::io::{Write, Result};

use crate::errors::syntax::{LocatedSyntaxError, self};
use crate::fmt::IndentingWriter;
use crate::ast::Ast;

pub struct CompileErrorRenderer<'a, const W: usize> {
    stream: IndentingWriter<'a, W>,
    ast: &'a Ast<'a>,
    source: &'a str,
    filename: &'a str,
    errors: &'a [LocatedSyntaxError],
}

impl <'a, const W: usize> CompileErrorRenderer<'a, W> {
    pub fn new(writer: &'a mut dyn Write, ast: &'a Ast<'a>, filename: &'a str, errors: &'a [LocatedSyntaxError]) -> CompileErrorRenderer<'a, W> {
        CompileErrorRenderer {
            stream: IndentingWriter::new(writer),
            ast,
            source: ast.source,
            filename,
            errors,
        }
    }

    pub fn format(&mut self) -> Result<()> {
        for src_error in self.errors {
            self.format_error(src_error)?;
        }

        Ok(())
    }

    fn skip_whitespace(&self, source_index: u32) -> u32 {
        let mut i = source_index as usize;
        while i < self.source.len() {
            if !(self.source.as_bytes()[i] as char).is_whitespace() {
                break;
            }

            i += 1;
        }

        return i as u32;
    }

    fn format_error(&mut self, err: &LocatedSyntaxError) -> Result<()> {
        let preview_message = "";

        let error_message = match err.error.tag {
            syntax::Tag::MissingColon => "missing colon",
            syntax::Tag::MissingReturnType => "missing a return type",
            syntax::Tag::MissingIdentifier => "missing an identifier",
            syntax::Tag::MissingTypeAnnotation => "missing a type annotation",
            syntax::Tag::UnexpectedTldToken => "unexpected statement",
            syntax::Tag::ExpectedExpression => "expected an expression",
            syntax::Tag::ExpectedClosingParen => "expected closing parentheses ')'",
        };

        self.stream.indent();

        self.stream.writer.write_fmt(format_args!("\x1b[3;36m{}\x1b[39m:{}:{} \x1b[0;31merror: {}", self.filename, err.line_number, err.column_number, error_message))?;
        self.stream.newline()?;

        let line_pos_nw = self.skip_whitespace(err.line_start) as usize;
        let skipped_ws = line_pos_nw - (err.line_start as usize);

        let mut next_line_start = err.line_start as usize;
        while self.source.as_bytes()[next_line_start] != b'\n' {
            next_line_start += 1;
        }
        self.stream.writer.write_fmt(format_args!("\x1b[0m{}{}", preview_message, &self.source[line_pos_nw..next_line_start]))?;
        self.stream.newline()?;

        let num_spaces = preview_message.len() + (err.column_number as usize);

        let num_spaces = if num_spaces >= skipped_ws {
            num_spaces - skipped_ws
        } else {
            num_spaces
        };
        let num_spaces = if num_spaces >= 1 {
            num_spaces - 1
        } else {
            num_spaces
        };

        for _ in 0..num_spaces { self.stream.writer.write_all(b" ")?; }
        self.stream.writer.write_all("\x1b[1;32m".as_bytes())?;
        for _ in 0..self.ast.token_string(err.error.token).len() { self.stream.writer.write_all(b"^")?; }
        self.stream.writer.write_all(" here\x1b[0m".as_bytes())?;
        self.stream.newline()?;

        self.stream.dedent();
        self.stream.newline()?;

        Ok(())
    }
}
