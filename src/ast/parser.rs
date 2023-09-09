use bumpalo::Bump;

use crate::lex::{token, token::Tag};
use crate::ast::node::{self, Node};
use crate::util::extra;
use crate::errors::syntax::{self, SyntaxError};

pub struct Parser<'a> {
    source: &'a str,
    tokens: token::CompactSlice<'a>,
    index: token::Index,

    pub nodes: Vec<Node>,
    pub extra: extra::Vec,
    scratch: bumpalo::collections::Vec<'a, extra::Data>,
    pub errors: Vec<SyntaxError>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken,
    HandledSourceError,
}

pub type Result<T> = std::result::Result<T, ParseError>;

impl <'a> Parser<'_> {
    pub fn new(bump: &'a Bump, source: &'a str, tokens: token::CompactSlice<'a>) -> Parser<'a> {
        Parser {
            source,
            tokens,
            index: token::Index::from(0),
            nodes: Vec::new(),
            extra: Vec::new(),
            scratch: bumpalo::collections::Vec::new_in(&bump),
            errors: Vec::new(),
        }
    }

    fn add_node(&mut self, node: Node) -> node::Index {
        assert!(self.nodes.len() <= (u32::MAX as usize));
        let result = self.nodes.len();
        self.nodes.push(node);
        return node::Index::from(result);
    }

    fn add_extra<T: extra::ExtraData<T>>(&mut self, data: T) -> extra::Index {
        let len = self.extra.len();
        assert!(len <= (u32::MAX as usize) - std::mem::size_of::<T>());
        data.pack(&mut self.extra);
        extra::Index::from(len)
    }

    fn token_tag(&self, index: token::Index) -> token::Tag {
        self.tokens[index].tag
    }

    fn current_tag(&self) -> token::Tag {
        self.token_tag(self.index)
    }

    fn eat_token(&mut self, tag: token::Tag) -> Option<token::Index> {
        if self.current_tag() == tag {
            self.index = self.index + 1;
            return Some(self.index - 1);
        } else {
            return None;
        }
    }

    fn expect_token(&mut self, tag: token::Tag) -> Result<token::Index> {
        match self.eat_token(tag) {
            Some(token) => Ok(token),
            None => Err(ParseError::UnexpectedToken),
        }
    }

    fn eat_current_token(&mut self) -> token::Index {
        assert!(self.current_tag() != Tag::Eof);
        self.index = self.index + 1;
        return self.index - 1;
    }

    fn consume_until_valid_tld(&mut self, report_unmatched: bool) {
        // consume until we're in a valid state e.g. next let
        let mut open_parenths: i32 = 0;
        let mut open_braces: i32 = 0;

        let mut last_open_parenth: token::Index = token::Index::from(0);
        let mut last_close_parenth: token::Index = token::Index::from(0);
        let mut last_open_brace: token::Index = token::Index::from(0);
        let mut last_close_brace: token::Index = token::Index::from(0);

        loop {
            match self.current_tag() {
                Tag::Eof => break,
                Tag::LeftParen => {
                    last_open_parenth = self.index;
                    open_parenths += 1;
                },
                Tag::RightParen => {
                    last_close_parenth = self.index;
                    open_parenths -= 1;
                },
                Tag::RightBrace => {
                    last_close_brace = self.index;
                    open_braces -= 1;
                },
                Tag::RightBrace => {
                    last_close_brace = self.index;
                    open_braces -= 1;
                },
                Tag::Let => {
                    // have to make sure this isn't in some smaller block
                    if (open_parenths == 0) && (open_braces == 0) {
                        break;
                    }
                },
                _ => {},
            }

            self.index = self.index + 1;
        }

        if report_unmatched {
            // TODO: report unmatched
        }
    }

    fn consume_until_semi(&mut self) {
        loop {
            match self.current_tag() {
                Tag::Semi => {
                    self.index = self.index + 1;
                    break;
                },
                Tag::Eof => break,
                _ => {},
            }

            self.index = self.index + 1;
        }
    }

    fn precedence(tag: token::Tag) -> i32 {
        match tag {
            Tag::Or => 10,
            Tag::And => 11,
            Tag::EqualEqual => 12,
            Tag::BangEqual => 12,
            Tag::LeftAngle => 13,
            Tag::RightAngle => 13,
            Tag::LeftAngleEqual => 13,
            Tag::RightAngleEqual => 13,
            // Tag::O => 20,
            // Tag::AmpersandAmpersand => 30,
            // Tag::CaretCaret => 40,
            Tag::Plus => 110,
            Tag::Minus => 120,
            Tag::Asterisk => 130,
            Tag::Slash => 140,
            Tag::Percent => 140,
            _ => -1,
        }
    }

    pub fn parse_module(&mut self) -> Result<node::Index> {
        // each toplevel (file) may create any number of global statements
        // so we collect the statement indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        let scratch_top = self.scratch.len();
        // TODO: defer drain

        loop {
            let node = match self.current_tag() {
                Tag::Eof => break,
                Tag::Let => match self.parse_declaration() {
                    Ok(node) => node,
                    Err(e) => match e {
                        ParseError::HandledSourceError => {
                            loop {
                                match self.current_tag() {
                                    Tag::Eof | Tag::Let => break,
                                    _ => {},
                                }
                                self.index = self.index + 1;
                            }

                            continue;
                        },
                        _ => return Err(e),
                    }
                },
                _ => {
                    self.errors.push(SyntaxError {
                        tag: syntax::Tag::UnexpectedTldToken,
                        token: self.index,
                    });
                    self.consume_until_valid_tld(false);
                    continue;
                }
                // _ => return Err(ParseError::UnexpectedToken),
            };

            self.scratch.push(node.into());
            _ = self.expect_token(Tag::Semi)?;
        }

        let stmts = &self.scratch[scratch_top..];
        let extra_top = self.extra.len();
        self.extra.extend_from_slice(stmts);
        self.scratch.drain(scratch_top..);

        Ok(self.add_node(Node {
            main_token: token::Index::from(0),
            data: node::Data::Module {
                stmts_start: extra::Index::from(extra_top),
                stmts_end: extra::Index::from(self.extra.len()),
            }
        }))
    }

    fn parse_expression(&mut self) -> Result<node::Index> {
        // declarations that aren't part of logical or arithmetic expressions,
        // like string literals, function and aggregate declarations, etc,
        // are parsed and returned here directly
        // for everything else, we part in parse_primary_expr() and try to
        // associate it with a binary companion (parse_bin_right_expr())
        match self.current_tag() {
            Tag::Fn => self.expect_fn_declaration(),
            _ => {
                let left_node = self.parse_primary_expression()?;
                return self.parse_bin_right_expr(left_node, 0);
            }
        }
    }

    fn parse_bin_right_expr(&mut self, l: node::Index, expr_precedence: i32) -> Result<node::Index> {
        // tries to associate an existing "left side" node with a right side
        // in one or more binary expressions - operator precedence parsing
        let mut l_node = l;
        loop {
            let prec = Parser::precedence(self.current_tag());
            if prec < expr_precedence {
                return Ok(l_node);
            }

            let op_token = self.eat_current_token();
            let mut r_node = self.parse_primary_expression()?;

            let next_prec = Parser::precedence(self.current_tag());
            if prec < next_prec {
                r_node = self.parse_bin_right_expr(r_node, prec + 1)?;
            }

            l_node = self.add_node(Node {
                main_token: op_token,
                data: node::Data::BinaryExpr { left: l_node, right: r_node },
            });
        }
    }

    fn parse_primary_expression(&mut self) -> Result<node::Index> {
        // parses an elementary exprsesion such as a literal,
        // variable value, function call, or parenthesis
        match self.current_tag() {
            Tag::LeftParen => {
                // parentheses are used only for grouping in source code
                // and don't generate ast nodes since the ast nesting itself
                // provides the correct grouping
                _ = self.expect_token(Tag::LeftParen).unwrap();
                let inner_node = self.parse_expression()?;
                match self.expect_token(Tag::RightParen) {
                    Ok(_) => {},
                    Err(_) => {
                        self.errors.push(SyntaxError {
                            tag: syntax::Tag::ExpectedClosingParen,
                            token: self.index,
                        });
                        return Err(ParseError::HandledSourceError);
                    }
                }

                Ok(inner_node)
            },
            Tag::Ident => match self.token_tag(self.index + 1) {
                // Tag::LeftParen => self.expect_call(),
                // TODO: implement calls
                _ => self.expect_var_expr(),
            },
            Tag::IntLit => {
                let main_token = self.eat_current_token();
                Ok(self.add_node(Node {
                    main_token,
                    data: node::Data::IntegerLiteral,
                }))
            },
            Tag::FloatLit => {
                let main_token = self.eat_current_token();
                Ok(self.add_node(Node {
                    main_token,
                    data: node::Data::FloatLiteral,
                }))
            },
            Tag::True | Tag::False => {
                let main_token = self.eat_current_token();
                Ok(self.add_node(Node {
                    main_token,
                    data: node::Data::BoolLiteral,
                }))
            },
            _ => {
                self.errors.push(SyntaxError {
                    tag: syntax::Tag::ExpectedExpression,
                    token: self.index,
                });
                Err(ParseError::HandledSourceError)
            },
        }
    }

    fn expect_var_expr(&mut self) -> Result<node::Index> {
        // parses variable identifier expressions (variable value)
        let ident_token = self.expect_token(Tag::Ident)?;
        Ok(self.add_node(Node {
            main_token: ident_token,
            data: node::Data::VarExpr,
        }))
    }

    fn expect_type(&mut self) -> Result<node::Index> {
        // parses a type as either a named identifier (u32, Point),
        // function prototype, or aggregate prototype
        match self.current_tag() {
            Tag::Ident => {
                let ident_token = self.eat_current_token();
                Ok(self.add_node(Node {
                    main_token: ident_token,
                    data: node::Data::NamedType,
                }))
            },
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn expect_fn_declaration(&mut self) -> Result<node::Index> {
        let fn_token = self.expect_token(Tag::Fn)?;
        let params = self.expect_parameter_list()?;

        let return_type = match self.expect_type() {
            Ok(ty) => ty,
            Err(e) => match e {
                ParseError::UnexpectedToken => {
                    self.errors.push(SyntaxError {
                        tag: crate::errors::syntax::Tag::MissingReturnType,
                        token: self.index,
                    });

                    return Err(ParseError::HandledSourceError);
                },
                ParseError::HandledSourceError => return Err(e),
            },
        };

        let body = self.expect_block()?;
        let signature = self.add_extra(node::FnSignature {
            params_start: params.start,
            params_end: params.end,
            return_type,
        });
        Ok(self.add_node(Node {
            main_token: fn_token,
            data: node::Data::FnDecl { signature, body },
        }))
    }

    fn expect_parameter_list(&mut self) -> Result<extra::Range> {
        _ = self.expect_token(Tag::LeftParen)?;

        // since each parameter may create multiple nodes (depending on type complexity)
        // we collect the toplevel parameter indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        let scratch_top = self.scratch.len();

        let mut handled_error = false;
        loop {
            match self.current_tag() {
                Tag::RightParen => {
                    self.index = self.index + 1;
                    break;
                },
                _ => {},
            }

            let param_node = match self.expect_parameter() {
                Ok(node) => node,
                Err(e) => match e {
                    ParseError::HandledSourceError => {
                        handled_error = true;
                        loop {
                            match self.current_tag() {
                                Tag::Comma => {
                                    self.index = self.index + 1;
                                    break;
                                },
                                Tag::RightParen | Tag::Eof => break,
                                _ => {},
                            }
                            self.index = self.index + 1;
                        }
                        continue;
                    },
                    ParseError::UnexpectedToken => unreachable!(),
                }
            };
            self.scratch.push(param_node.into());
            match self.current_tag() {
                Tag::Comma => self.index = self.index + 1,
                Tag::RightParen => {
                    self.index = self.index + 1;
                    break;
                },
                _ => {
                    self.scratch.drain(scratch_top..);
                    return Err(ParseError::UnexpectedToken);
                },
            }
        }

        if handled_error {
            self.scratch.drain(scratch_top..);
            return Err(ParseError::HandledSourceError);
        }

        let params = &self.scratch[scratch_top..];
        let extra_top = self.extra.len();
        self.extra.extend_from_slice(params);
        self.scratch.drain(scratch_top..);

        Ok(extra::Range {
            start: extra::Index::from(extra_top),
            end: extra::Index::from(self.extra.len()),
        })
    }

    fn expect_parameter(&mut self) -> Result<node::Index> {
        let ident_token = match self.expect_token(Tag::Ident) {
            Ok(token) => token,
            Err(e) => return Err(e),
        };

        let mut missing_colon = false;
        match self.expect_token(Tag::Colon) {
            Ok(_) => {},
            Err(e) => match e {
                ParseError::UnexpectedToken => missing_colon = true,
                _ => unreachable!(),
            }
        };
        let type_node = match self.expect_type() {
            Ok(node) => Some(node),
            Err(e) => match e {
                ParseError::UnexpectedToken => None,
                _ => unimplemented!(),
            }
        };

        if missing_colon {
            match type_node {
                Some(_) => {
                    // just missing a colon, but have the actual type
                    self.errors.push(SyntaxError {
                        tag: syntax::Tag::MissingColon,
                        token: self.index - 1,
                    });
                },
                None => {
                    // missing the entire type annotation
                    self.errors.push(SyntaxError {
                        tag: syntax::Tag::MissingTypeAnnotation,
                        token: self.index,
                    });
                },
            }

            return Err(ParseError::HandledSourceError);
        }

        Ok(self.add_node(Node {
            main_token: ident_token,
            data: node::Data::Param { ty: type_node.unwrap() },
        }))
    }

    fn expect_block(&mut self) -> Result<node::Index> {
        let l_brace_token = self.expect_token(Tag::LeftBrace)?;

        // since each block may create an arbitrary number of statements,
        // we collect the toplevel statement indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        let scratch_top = self.scratch.len();

        loop {
            match self.eat_token(Tag::RightBrace) {
                Some(_) => break,
                _ => {},
            }
            let statement = self.parse_statement(true)?;
            self.scratch.push(statement.into());
        }

        let stmts = &self.scratch[scratch_top..];
        let extra_top = self.extra.len();
        self.extra.extend_from_slice(stmts);
        self.scratch.drain(scratch_top..);

        Ok(self.add_node(Node {
            main_token: l_brace_token,
            data: node::Data::Block {
                stmts_start: extra::Index::from(extra_top),
                stmts_end: extra::Index::from(self.extra.len())
            },
        }))
    }

    fn parse_statement(&mut self, eat_semi: bool) -> Result<node::Index> {
        let node = match self.current_tag() {
            Tag::Let => self.parse_declaration()?,
            Tag::Return => self.expect_return_statement()?,
            Tag::If => return self.parse_conditional(),
            Tag::For => return self.parse_loop(),
            Tag::Ident => match self.token_tag(self.index + 1) {
                Tag::LeftParen => self.expect_call()?,
                Tag::Equal | Tag::PlusEqual | Tag::MinusEqual | Tag::AsteriskEqual | Tag::SlashEqual | Tag::PercentEqual | Tag::AmpersandEqual | Tag::PipeEqual | Tag::CaretEqual | Tag::LeftAngleLeftAngleEqual | Tag::RightAngleRightAngleEqual => self.parse_assignment()?,
                _ => return Err(ParseError::UnexpectedToken),
            }
            Tag::Break => self.expect_break()?,
            _ => return Err(ParseError::UnexpectedToken),
        };

        if eat_semi {
            _ = self.expect_token(Tag::Semi)?;
        }

        return Ok(node);
    }

    fn parse_declaration(&mut self) -> Result<node::Index> {
        let let_token = self.expect_token(Tag::Let)?;
        if self.current_tag() == Tag::Mut {
            _ = self.eat_token(Tag::Mut);
            _ = self.expect_token(Tag::Ident)?;

            let ty = match self.eat_token(Tag::Colon) {
                Some(_) => self.expect_type()?,
                None => 0u32.into(),
            };
            _ = self.expect_token(Tag::Equal)?;

            match self.parse_expression() {
                Ok(val) => Ok(self.add_node(Node {
                    main_token: let_token,
                    data: node::Data::VarDecl { ty, val }
                })),
                Err(e) => {
                    Err(e)
                },
            }
        } else {
            _ = self.expect_token(Tag::Ident)?;

            let ty = match self.eat_token(Tag::Colon) {
                Some(_) => self.expect_type()?,
                None => 0u32.into(),
            };
            _ = self.expect_token(Tag::Equal)?;

            match self.parse_expression() {
                Ok(val) => Ok(self.add_node(Node {
                    main_token: let_token,
                    data: node::Data::ConstDecl { ty, val }
                })),
                Err(e) => Err(e),
            }
        }
    }

    fn parse_type_declaration(&mut self) -> Result<node::Index> {
        // parses a type declaration (type Point = ...)
        let type_token = self.expect_token(Tag::Type)?;
        _ = self.expect_token(Tag::Ident);
        _ = self.expect_token(Tag::Equal);
        let type_node = self.expect_type()?;

        Ok(self.add_node(Node {
            main_token: type_token,
            data: node::Data::TypeDecl { ty: type_node },
        }))
    }

    fn expect_call(&mut self) -> Result<node::Index> {
        let ident_token = self.expect_token(Tag::Ident)?;
        let args = self.expect_argument_list()?;

        Ok(self.add_node(Node {
            main_token:  ident_token,
            data: node::Data::CallExpr { args_start: args.start, args_end: args.end },
        }))
    }

    fn expect_argument_list(&mut self) -> Result<extra::Range> {
        _ = self.expect_token(Tag::LeftParen)?;

        // since each argument may create arbitrarily many nodes
        // (arguments can be inline exprsesions),
        // we collect the toplevel argument indices in the scratch list,
        // append all of them to extra at the end, and return the
        // range in extra containing those indices
        let scratch_top = self.scratch.len();
        // TODO: defer
        loop {
            match self.eat_token(Tag::RightParen) {
                Some(_) => break,
                None => {},
            }

            let arg_node = self.parse_expression()?;
            self.scratch.push(arg_node.into());
            match self.current_tag() {
                Tag::Comma => _ = self.eat_token(Tag::Comma),
                Tag::RightParen => {},
                _ => return Err(ParseError::UnexpectedToken),
            }
        }

        let params = &self.scratch[scratch_top..];
        let extra_top = self.extra.len();
        self.extra.extend_from_slice(params);
        self.scratch.drain(scratch_top..);

        Ok(extra::Range {
            start: extra::Index::from(extra_top),
            end: extra::Index::from(self.extra.len()),
        })
    }

    fn parse_assignment(&mut self) -> Result<node::Index> {
        let ident_token = self.expect_token(Tag::Ident)?;

        match self.current_tag() {
            Tag::Equal => {
                _ = self.expect_token(Tag::Equal)?;
                let val = self.parse_expression()?;

                Ok(self.add_node(Node {
                    main_token: ident_token,
                    data: node::Data::AssignSimple { val },
                }))
            },
            _ => {
                self.index = self.index + 1;
                let val = self.parse_expression()?;

                Ok(self.add_node(Node {
                    main_token: ident_token,
                    data: node::Data::AssignBinary { val },
                }))
            }
        }
    }

    fn parse_conditional(&mut self) -> Result<node::Index> {
        // all conditional branches start with the if keyword
        let if_token = self.expect_token(Tag::If)?;

        // we have three kinds of if statements: simple, else, and chain
        // which we progressively try to match against
        let condition = self.parse_expression()?;
        let exec_true = self.expect_block()?;

        match self.eat_token(Tag::Else) {
            Some(_) => {
                match self.current_tag() {
                    Tag::If => {
                        // chained if
                        let next = self.parse_conditional()?;
                        let chain = self.add_extra(node::IfChain { exec_true, next });

                        Ok(self.add_node(Node {
                            main_token: if_token,
                            data: node::Data::IfChain { condition, chain },
                        }))
                    },
                    _ => {
                        let exec_false = self.expect_block()?;
                        let exec = self.add_extra(node::IfElse { exec_true, exec_false });

                        Ok(self.add_node(Node {
                            main_token: if_token,
                            data: node::Data::IfElse { condition, exec },
                        }))
                    }
                }
            },
            None => Ok(self.add_node(Node {
                main_token: if_token,
                data: node::Data::IfSimple { condition, exec_true },
            }))
        }
    }

    fn parse_loop(&mut self) -> Result<node::Index> {
        // all loops start with the for keyword
        let for_token = self.expect_token(Tag::For)?;

        // we have three kinds of loops: forever, conditional, range
        // which we progressively try to match against
        match self.current_tag() {
            Tag::LeftBrace => {
                // forever loop
                let body = self.expect_block()?;

                Ok(self.add_node(Node {
                    main_token: for_token,
                    data: node::Data::LoopForever { body },
                }))
            },
            Tag::Let => {
                // declaration = assume this is the binding, and we are in a range loop
                let binding = self.parse_declaration()?;
                _ = self.expect_token(Tag::Semi)?;
                let condition = self.parse_expression()?;
                _ = self.expect_token(Tag::Semi)?;
                let afterthought = self.parse_statement(false)?;

                let signature = self.add_extra(node::RangeSignature {
                    binding, condition, afterthought,
                });
                let body = self.expect_block()?;

                Ok(self.add_node(Node {
                    main_token: for_token,
                    data: node::Data::LoopRange { signature, body },
                }))
            },
            _ => {
                // assume this is the condition of a conditional loop
                let condition = self.parse_expression()?;
                let body = self.expect_block()?;

                Ok(self.add_node(Node {
                    main_token: for_token,
                    data: node::Data::LoopConditional { condition, body },
                }))
            }
        }
    }

    fn expect_break(&mut self) -> Result<node::Index> {
        let break_token = self.expect_token(Tag::Break)?;

        Ok(self.add_node(Node {
            main_token: break_token,
            data: node::Data::Break,
        }))
    }

    fn expect_return_statement(&mut self) -> Result<node::Index> {
        let ret_token = self.expect_token(Tag::Return)?;

        match self.current_tag() {
            Tag::Semi => {
                // no return value, assumed void function (will verify in IR during type checking)
                Ok(self.add_node(Node {
                    main_token: ret_token,
                    data: node::Data::ReturnVal { val: 0u32.into() },
                }))
            },
            _ => {
                let expr_node = self.parse_expression()?;
                Ok(self.add_node(Node {
                    main_token: ret_token,
                    data: node::Data::ReturnVal { val: expr_node },
                }))
            }
        }
    }
}
