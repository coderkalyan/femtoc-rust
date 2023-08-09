pub mod syntax {
    use crate::lex::token;
    use crate::ast;

    #[derive(Debug, Clone)]
    pub struct SyntaxError {
        pub tag: Tag,
        pub token: token::Index,
    }

    #[derive(Debug, Clone)]
    pub enum Tag {
        MissingColon,
        MissingTypeAnnotation,
        MissingReturnType,
        MissingIdentifier,
        UnexpectedTldToken,
    }

    #[derive(Debug)]
    pub struct LocatedSyntaxError {
        pub error: SyntaxError,
        pub line_number: u32,
        pub column_number: u32,
        pub line_start: u32,
    }

    pub fn locate_errors(ast: &ast::Ast, src_errors: &[SyntaxError]) -> Vec<LocatedSyntaxError> {
        let mut errors = Vec::<LocatedSyntaxError>::new();
        errors.reserve(src_errors.len());

        for src_error in src_errors.iter() {
            errors.push(LocatedSyntaxError {
                error: src_error.clone(),
                line_number: 0,
                column_number: 0,
                line_start: 0,
            });
        }
        errors.sort_by_key(|error| error.error.token);

        let mut error_index: usize = 0;
        let mut byte_offset: usize = 0;
        let mut line_number: u32 = 0;
        let mut column_number: u32 = 0;
        let mut line_start: usize = 0;
        while byte_offset < ast.source.len() {
            if error_index >= errors.len() {
                break;
            }

            if ast.source.as_bytes()[byte_offset] == b'\n' {
                line_number += 1;
                column_number = 0;
                line_start = byte_offset + 1;
            } else {
                column_number += 1;
            }

            let mut token = errors[error_index].error.token;
            while ast.token_start(token) == byte_offset {
                errors[error_index].line_number = line_number;
                errors[error_index].column_number = column_number;
                errors[error_index].line_start = line_start as u32;
                error_index += 1;
                if error_index >= errors.len() {
                    break;
                }
                token = errors[error_index].error.token;
            }

            byte_offset += 1;
        }

        errors
    }
}
