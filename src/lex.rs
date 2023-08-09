use phf::phf_map;

use crate::lex::token::{Token, Tag, Loc};

pub mod token {
    // each token contains a type (tag), start index, and end index in the source string
    pub struct Token {
        pub tag: Tag,
        pub loc: Loc,
    }

    pub struct Loc {
        pub start: u32,
        pub end: u32,
    }

    #[derive(Clone, Copy, Eq, PartialEq, Debug)]
    pub enum Tag {
        // lexer flow control
        Invalid,
        Eof,

        Ident,
        StrLit,
        CharLit,
        IntLit,
        FloatLit,

        // single character punctuation
        // generic
        Semi,
        Colon,
        Equal,
        Period,
        Comma,
        Underscore,
        // grouping
        LeftParen,
        RightParen,
        LeftBracket,
        RightBracket,
        LeftBrace,
        RightBrace,
        // arithmetic
        Plus,
        Minus,
        Asterisk,
        Slash,
        Percent,
        // binary
        Ampersand,
        Pipe,
        Caret,
        Tilde,
        // logical
        Bang,
        // comparison
        LeftAngle,
        RightAngle,

        // double character punctuation
        // arithmetic
        PlusEqual,
        MinusEqual,
        AsteriskEqual,
        SlashEqual,
        PercentEqual,
        // binary
        AmpersandEqual,
        PipeEqual,
        CaretEqual,
        LeftAngleLeftAngle,
        RightAngleRightAngle,
        // logical
        // comparison
        EqualEqual,
        LeftAngleEqual,
        RightAngleEqual,
        BangEqual,

        // triple character punctuation
        // binary
        LeftAngleLeftAngleEqual,
        RightAngleRightAngleEqual,

        // keywords
        Use,
        As,
        Fn,
        Return,
        Let,
        Mut,
        Type,
        If,
        Else,
        Yield,
        Struct,
        Enum,
        Variant,
        Defer,
        For,
        Break,
        Or,
        And,
        Not,
        True,
        False,
    }

    #[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord)]
    pub struct Index(u32);

    impl Index {
        pub fn from(index: u32) -> Index {
            Index(index)
        }

        pub fn value(&self) -> usize {
            self.0 as usize
        }
    }

    impl std::ops::Add<u32> for Index {
        type Output = Index;

        fn add(self, rhs: u32) -> Index {
            Index(self.0 + rhs)
        }
    }

    impl std::ops::Sub<u32> for Index {
        type Output = Index;

        fn sub(self, rhs: u32) -> Index {
            Index(self.0 - rhs)
        }
    }

    pub struct Slice<'a> (&'a [Token]);

    impl <'a> std::ops::Index<Index> for Slice<'a> {
        type Output = Token;

        fn index(&self, index: Index) -> &'a Token {
            &self.0[index.value()]
        }
    }

    impl <'a> Slice<'_> {
        pub fn from(vec: &'a Vec<Token>) -> Slice<'a> {
            Slice(vec.as_slice())
        }
    }

    #[derive(Debug)]
    pub struct CompactToken {
        pub tag: Tag,
        pub start: u32,
    }

    #[derive(Clone, Copy)]
    pub struct CompactSlice<'a> (&'a [CompactToken]);

    impl <'a> std::ops::Index<Index> for CompactSlice<'a> {
        type Output = CompactToken;

        fn index(&self, index: Index) -> &'a CompactToken {
            &self.0[index.value()]
        }
    }

    impl <'a> CompactSlice<'_> {
        pub fn from(vec: &'a Vec<CompactToken>) -> CompactSlice<'a> {
            CompactSlice(vec.as_slice())
        }
    }
}

static KEYWORDS: phf::Map<&'static str, Tag> = phf_map! {
    "use" => Tag::Use,
    "as" => Tag::As,
    "fn" => Tag::Fn,
    "return" => Tag::Return,
    "let" => Tag::Let,
    "mut" => Tag::Mut,
    "type" => Tag::Type,
    "if" => Tag::If,
    "else" => Tag::Else,
    "yield" => Tag::Yield,
    "struct" => Tag::Struct,
    "enum" => Tag::Enum,
    "variant" => Tag::Variant,
    "defer" => Tag::Defer,
    "for" => Tag::For,
    "break" => Tag::Break,
    "or" => Tag::Or,
    "and" => Tag::And,
    "not" => Tag::Not,
    "true" => Tag::True,
    "false" => Tag::False,
};

fn get_keyword(bytes: &str) -> Option<Tag> {
    KEYWORDS.get(bytes).cloned()
}

enum State {
    Start,

    // named things
    Ident,
    Underscore,

    // literals
    StrLit,
    IntBase,
    Binary,
    Octal,
    Decimal,
    Hex,
    FloatBase,
    FloatExp,

    // partial operators
    Period,
    Equal,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Bang,
    LeftAngle,
    RightAngle,
    LeftAngleLeftAngle,
    RightAngleRightAngle,

    LineComment,
}

pub struct Lexer<'a> {
    // source code being analyzed
    source: &'a str,
    // current lexer index in source
    index: usize,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer::new_index(source, 0)
    }

    pub fn new_index(source: &str, index: u32) -> Lexer {
        assert!(source.len() <= (u32::MAX as usize));

        Lexer {
            source: source,
            index: index as usize,
        }
    }

    fn getchar(&self) -> char {
        if self.index < self.source.len() {
            self.source.as_bytes()[self.index] as char
        } else {
            '\0'
        }
    }

    pub fn next(&mut self) -> Token {
        let mut state = State::Start;
        let mut result = token::Token {
            tag: Tag::Eof,
            loc: Loc {
                start: self.index.try_into().unwrap(),
                end: 0,
            },
        };

        // finite state machine that parses one character at a time
        // the state starts with .start, where we parse (almost) any
        // character and decide what to do with it
        // 0 = null terminator
        // single character tokens are returned immediately, while
        // multi character tokens (multi character operators, keywords,
        // and identifiers) require intermediate states
        //
        // if the current character ends a token (inclusive), we increment
        // the index to set the correct end location, and break
        // current characters signaling the end of a previous token (exclusive)
        // break but don't increment the index
        // else, the while loop will increment automatically
        loop {
            // switch on the state, and then the current character c
            match state {
                State::Start => {
                    match self.getchar() {
                        // eof
                        '\0' => break,

                        // whitespace
                        ' ' | '\n' | '\r' | '\t' => result.loc.start = (self.index + 1) as u32,

                        // identifier
                        'a'..='z' | 'A'..='Z' => {
                            state = State::Ident;
                            result.tag = Tag::Ident;
                        },
                        '_' => {
                            state = State::Underscore;
                            result.tag = Tag::Ident;
                        },

                        // string literal
                        '"' => {
                            state = State::StrLit;
                            result.tag = Tag::StrLit;
                        },
                        // char literal
                        '\'' => {
                            self.index += 1;
                            if self.getchar() == '\'' {
                                self.index += 1;
                                result.tag = Tag::Invalid;
                                break;
                            }
                            if self.getchar() != '\0' {
                                self.index += 1;
                            }
                            if self.getchar() != '\'' {
                                result.tag = Tag::Invalid;
                                loop {
                                    match self.getchar() {
                                        '\'' | '\0' => break,
                                        _ => {},
                                    }
                                    self.index += 1;
                                }
                            } else {
                                result.tag = Tag::CharLit;
                            }

                            // eat closing quote
                            if self.getchar() == '\'' {
                                self.index += 1;
                            }
                            break;
                        },
                        // number literal
                        '0' => state = State::IntBase,
                        '1'..='9' => state = State::Decimal,

                        // punctuation
                        ';' => {
                            result.tag = Tag::Semi;
                            self.index += 1;
                            break;
                        },
                        '=' => state = State::Equal,
                        '.' => state = State::Period,
                        ':' => {
                            result.tag = Tag::Colon;
                            self.index += 1;
                            break;
                        },
                        ',' => {
                            result.tag = Tag::Comma;
                            self.index += 1;
                            break;
                        },
                        '(' => {
                            result.tag = Tag::LeftParen;
                            self.index += 1;
                            break;
                        },
                        ')' => {
                            result.tag = Tag::RightParen;
                            self.index += 1;
                            break;
                        },
                        '[' => {
                            result.tag = Tag::LeftBracket;
                            self.index += 1;
                            break;
                        },
                        ']' => {
                            result.tag = Tag::RightBracket;
                            self.index += 1;
                            break;
                        },
                        '{' => {
                            result.tag = Tag::LeftBrace;
                            self.index += 1;
                            break;
                        },
                        '}' => {
                            result.tag = Tag::RightBrace;
                            self.index += 1;
                            break;
                        },
                        '+' => state = State::Plus,
                        '-' => state = State::Minus,
                        '*' => state = State::Asterisk,
                        '/' => state = State::Slash,
                        '%' => state = State::Percent,
                        '&' => state = State::Ampersand,
                        '|' => state = State::Pipe,
                        '^' => state = State::Caret,
                        '~' => {
                            result.tag = Tag::Tilde;
                            self.index += 1;
                            break;
                        },
                        '!' => state = State::Bang,
                        '<' => state = State::LeftAngle,
                        '>' => state = State::RightAngle,
                        _ => {
                            result.tag = Tag::Invalid;
                            self.index += 1;
                            break;
                        },
                    }
                },
                State::Ident => match self.getchar() {
                    'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {},
                    _ => {
                        // done with identifier, check if keyword
                        let start = result.loc.start as usize;
                        match get_keyword(&self.source[start..self.index]) {
                            Some(tag) => result.tag = tag,
                            None => {}, // remains as ident
                        }
                        break;
                    },
                },
                State::StrLit => match self.getchar() {
                    '"' => {
                        self.index += 1;
                        break;
                    },
                    _ => {},
                },
                State::IntBase => match self.getchar() {
                    'b' => state = State::Binary,
                    'o' => state = State::Octal,
                    'x' => state = State::Hex,
                    '0'..='9' => state = State::Decimal,
                    '.' => state = State::FloatBase,
                    'a' | 'c'..='n' | 'p'..='w' | 'y'..='z' | 'A'..='Z' => {
                        self.index += 1;
                        let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::IntLit;
                        break;
                    },
                },
                State::Binary => match self.getchar() {
                    '0'..='1' | '_' => {},
                    '2'..='9' | 'a'..='z' | 'A'..='Z' => {
                        // let mut invalid_length = 1;
                        loop {
                            self.index += 1;
                            match self.getchar() {
                                '2'..='9' | 'a'..='z' | 'A'..='Z' => {},
                                _ => break, // includes EOF
                            }
                        }
                        // self.index += 1;
                        // let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        // self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::IntLit;
                        break;
                    },
                },
                State::Octal => match self.getchar() {
                    '0'..='7' | '_' => {},
                    '8'..='9' | 'a'..='z' | 'A'..='Z' => {
                        self.index += 1;
                        let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::IntLit;
                        break;
                    },
                },
                State::Decimal => match self.getchar() {
                    '0'..='9' | '_' => {},
                    '.' | 'e' => state = State::FloatBase,
                    'a'..='d' | 'f'..='z' | 'A'..='Z' => {
                        self.index += 1;
                        let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::IntLit;
                        break;
                    },
                },
                State::Hex => match self.getchar() {
                    '0'..='9' | 'a'..='f' | 'A'..='F' | '_' => {},
                    'g'..='z' | 'G'..='Z' => {
                        self.index += 1;
                        let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::IntLit;
                        break;
                    },
                },
                State::FloatBase => match self.getchar() {
                    '0'..='9' | '_' => {},
                    'e' => {
                        state = State::FloatExp;
                        self.index += 1;
                        match self.getchar() {
                            '+' | '-' => {},
                            _ => self.index -= 1,
                        }
                    }
                    'a'..='d' | 'f'..='z' | 'G'..='Z' => {
                        self.index += 1;
                        let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::FloatLit;
                        break;
                    },
                },
                State::FloatExp => match self.getchar() {
                    '0'..='9' | '_' => {},
                    'a'..='z' | 'A'..='Z' => {
                        self.index += 1;
                        let invalid_length = self.eat_invalid_literal();
                        result.tag = Tag::Invalid;
                        self.index += invalid_length;
                        break;
                    },
                    _ => {
                        result.tag = Tag::FloatLit;
                        break;
                    },
                },
                State::Underscore => match self.getchar() {
                    'a'..='z' | 'A'..='Z' => {
                        result.tag = Tag::Ident;
                        state = State::Ident;
                    },
                    _ => {
                        result.tag = Tag::Underscore;
                        break;
                    },
                },
                State::Period => match self.getchar() {
                    '0'..='9' => {
                        state = State::FloatBase;
                    },
                    _ => {
                        result.tag = Tag::Period;
                        break;
                    },
                },
                State::Equal => match self.getchar() {
                    '=' => {
                        result.tag = Tag::EqualEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Equal;
                        break;
                    },
                },
                State::Plus => match self.getchar() {
                    '=' => {
                        result.tag = Tag::PlusEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Plus;
                        break;
                    },
                },
                State::Minus => match self.getchar() {
                    '=' => {
                        result.tag = Tag::MinusEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Minus;
                        break;
                    },
                },
                State::Asterisk => match self.getchar() {
                    '=' => {
                        result.tag = Tag::AsteriskEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Asterisk;
                        break;
                    },
                },
                State::Slash => match self.getchar() {
                    '=' => {
                        result.tag = Tag::SlashEqual;
                        self.index += 1;
                        break;
                    },
                    '/' => state = State::LineComment,
                    _ => {
                        result.tag = Tag::Slash;
                        break;
                    },
                },
                State::Percent => match self.getchar() {
                    '=' => {
                        result.tag = Tag::PercentEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Percent;
                        break;
                    },
                },
                State::Ampersand => match self.getchar() {
                    '=' => {
                        result.tag = Tag::AmpersandEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Ampersand;
                        break;
                    },
                },
                State::Pipe => match self.getchar() {
                    '=' => {
                        result.tag = Tag::PipeEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Pipe;
                        break;
                    },
                },
                State::Caret => match self.getchar() {
                    '=' => {
                        result.tag = Tag::CaretEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Caret;
                        break;
                    },
                },
                State::Bang => match self.getchar() {
                    '=' => {
                        result.tag = Tag::BangEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::Bang;
                        break;
                    },
                },
                State::LeftAngle => match self.getchar() {
                    '=' => {
                        result.tag = Tag::LeftAngleEqual;
                        self.index += 1;
                        break;
                    },
                    '<' => state = State::LeftAngleLeftAngle,
                    _ => {
                        result.tag = Tag::LeftAngle;
                        break;
                    },
                },
                State::RightAngle => match self.getchar() {
                    '=' => {
                        result.tag = Tag::RightAngleEqual;
                        self.index += 1;
                        break;
                    },
                    '>' => state = State::RightAngleRightAngle,
                    _ => {
                        result.tag = Tag::RightAngle;
                        break;
                    },
                },
                State::LeftAngleLeftAngle => match self.getchar() {
                    '=' => {
                        result.tag = Tag::LeftAngleLeftAngleEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::LeftAngleLeftAngle;
                        break;
                    },
                },
                State::RightAngleRightAngle => match self.getchar() {
                    '=' => {
                        result.tag = Tag::RightAngleRightAngleEqual;
                        self.index += 1;
                        break;
                    },
                    _ => {
                        result.tag = Tag::RightAngleRightAngle;
                        break;
                    },
                },
                State::LineComment => match self.getchar() {
                    '\n' => {
                        result.loc.start = (self.index + 1) as u32;
                        state = State::Start;
                    },
                    '\0' => break,
                    _ => {},
                },
            }

            self.index += 1;
        }

        if result.tag == Tag::Eof {
            result.loc.start = self.index as u32;
        }

        result.loc.end = self.index.try_into().unwrap();
        return result;
    }

    fn eat_invalid_literal(&mut self) -> usize {
        let mut length = 0;

        while (self.index + length) < self.source.len() {
            match self.source.as_bytes()[self.index + length] as char {
                '0'..='9' | 'a'..='z' | 'A'..='Z' => {},
                _ => return length,
            }

            length += 1;
        }

        return length;
    }
}

#[cfg(test)]
mod tests {
    use crate::lex::{Lexer, token::Tag};

    fn test_lex(source: &str, expected_token_tags: &[Tag]) {
        let mut lexer = Lexer::new(source);
        for expected_token_tag in expected_token_tags.iter().cloned() {
            let token = lexer.next();
            assert_eq!(expected_token_tag, token.tag);
        }

        let eof = lexer.next();
        assert_eq!(Tag::Eof, eof.tag);
        assert_eq!(source.len() as u32, eof.loc.start);
        assert_eq!(source.len() as u32, eof.loc.end);
    }

    #[test]
    fn identifier() {
        test_lex("x", &[Tag::Ident]);
        test_lex("abc", &[Tag::Ident]);
        test_lex("a123", &[Tag::Ident]);
        test_lex("123a", &[Tag::Invalid]);
        test_lex("1abc", &[Tag::Invalid]);
    }

    #[test]
    fn string_literal() {
        test_lex("\"abc\"", &[Tag::StrLit]);
        test_lex("\"\"", &[Tag::StrLit]);
        test_lex("\"123456\"", &[Tag::StrLit]);
        test_lex("\"let\"", &[Tag::StrLit]);
    }
    
    #[test]
    fn character_literal() {
        // TODO: special characters (escaped)
        test_lex("'a'", &[Tag::CharLit]);
        test_lex("'0'", &[Tag::CharLit]);
        test_lex("'abc'", &[Tag::Invalid]);
        test_lex("''", &[Tag::Invalid]);
    }

    #[test]
    fn integer_literal() {
        // decimal
        test_lex("0", &[Tag::IntLit]);
        test_lex("123", &[Tag::IntLit]);
        test_lex("123(", &[Tag::IntLit, Tag::LeftParen]);
        test_lex("123;", &[Tag::IntLit, Tag::Semi]);
        test_lex("123abc", &[Tag::Invalid]);
        test_lex("123_456", &[Tag::IntLit]);
        test_lex("123_456_789", &[Tag::IntLit]);

        // binary
        test_lex("0b0", &[Tag::IntLit]);
        test_lex("0b1", &[Tag::IntLit]);
        test_lex("0b0101011", &[Tag::IntLit]);
        test_lex("0b1234", &[Tag::Invalid]);

        // octal
        test_lex("0o0", &[Tag::IntLit]);
        test_lex("0o1", &[Tag::IntLit]);
        test_lex("0o0123456", &[Tag::IntLit]);
        test_lex("0o17", &[Tag::IntLit]);
        test_lex("0o178", &[Tag::Invalid]);
        test_lex("0o17abc", &[Tag::Invalid]);
        test_lex("0o12345_67", &[Tag::IntLit]);

        // hex
        test_lex("0x0", &[Tag::IntLit]);
        test_lex("0x1", &[Tag::IntLit]);
        test_lex("0x018ADFF", &[Tag::IntLit]);
        test_lex("0x0123456", &[Tag::IntLit]);
        test_lex("0x789", &[Tag::IntLit]);
        test_lex("0xabcdef", &[Tag::IntLit]);
        test_lex("0xABCDEF", &[Tag::IntLit]);
        test_lex("0x123G", &[Tag::Invalid]);
        test_lex("0x12_3456_789_a_b_CDEF", &[Tag::IntLit]);
    }

    #[test]
    fn float_literal() {
        test_lex(".", &[Tag::Period]);
        test_lex(".5", &[Tag::FloatLit]);
        test_lex("5.", &[Tag::FloatLit]);
        test_lex("0.5", &[Tag::FloatLit]);
        test_lex("1.5", &[Tag::FloatLit]);
        test_lex(".51231232", &[Tag::FloatLit]);
        test_lex(".51231232e05", &[Tag::FloatLit]);
        test_lex(".51231232e+15", &[Tag::FloatLit]);
        test_lex(".51231232e-15", &[Tag::FloatLit]);
        test_lex(".5_234_12_32e-10", &[Tag::FloatLit]);
    }

    #[test]
    fn punctuation() {
        test_lex(";", &[Tag::Semi]);
        test_lex(":", &[Tag::Colon]);
        test_lex("=", &[Tag::Equal]);
        test_lex(".", &[Tag::Period]);
        test_lex(",", &[Tag::Comma]);
        test_lex("_", &[Tag::Underscore]);
        test_lex("(", &[Tag::LeftParen]);
        test_lex(")", &[Tag::RightParen]);
        test_lex("[", &[Tag::LeftBracket]);
        test_lex("]", &[Tag::RightBracket]);
        test_lex("{", &[Tag::LeftBrace]);
        test_lex("}", &[Tag::RightBrace]);
        test_lex("+", &[Tag::Plus]);
        test_lex("-", &[Tag::Minus]);
        test_lex("*", &[Tag::Asterisk]);
        test_lex("/", &[Tag::Slash]);
        test_lex("%", &[Tag::Percent]);
        test_lex("&", &[Tag::Ampersand]);
        test_lex("|", &[Tag::Pipe]);
        test_lex("^", &[Tag::Caret]);
        test_lex("~", &[Tag::Tilde]);
        test_lex("!", &[Tag::Bang]);
        test_lex("<", &[Tag::LeftAngle]);
        test_lex(">", &[Tag::RightAngle]);

        test_lex("+=", &[Tag::PlusEqual]);
        test_lex("+ =", &[Tag::Plus, Tag::Equal]);
        test_lex("-=", &[Tag::MinusEqual]);
        test_lex("- =", &[Tag::Minus, Tag::Equal]);
        test_lex("*=", &[Tag::AsteriskEqual]);
        test_lex("* =", &[Tag::Asterisk, Tag::Equal]);
        test_lex("/=", &[Tag::SlashEqual]);
        test_lex("/ =", &[Tag::Slash, Tag::Equal]);
        test_lex("%=", &[Tag::PercentEqual]);
        test_lex("% =", &[Tag::Percent, Tag::Equal]);
        test_lex("&=", &[Tag::AmpersandEqual]);
        test_lex("& =", &[Tag::Ampersand, Tag::Equal]);
        test_lex("|=", &[Tag::PipeEqual]);
        test_lex("| =", &[Tag::Pipe, Tag::Equal]);
        test_lex("^=", &[Tag::CaretEqual]);
        test_lex("^ =", &[Tag::Caret, Tag::Equal]);
        test_lex("<<", &[Tag::LeftAngleLeftAngle]);
        test_lex("< <", &[Tag::LeftAngle, Tag::LeftAngle]);
        test_lex(">>", &[Tag::RightAngleRightAngle]);
        test_lex("> >", &[Tag::RightAngle, Tag::RightAngle]);
        test_lex("==", &[Tag::EqualEqual]);
        test_lex("= =", &[Tag::Equal, Tag::Equal]);
        test_lex("<=", &[Tag::LeftAngleEqual]);
        test_lex("< =", &[Tag::LeftAngle, Tag::Equal]);
        test_lex(">=", &[Tag::RightAngleEqual]);
        test_lex("> =", &[Tag::RightAngle, Tag::Equal]);
        test_lex("!=", &[Tag::BangEqual]);
        test_lex("! =", &[Tag::Bang, Tag::Equal]);

        test_lex("<<=", &[Tag::LeftAngleLeftAngleEqual]);
        test_lex("<< =", &[Tag::LeftAngleLeftAngle, Tag::Equal]);
        test_lex(">>=", &[Tag::RightAngleRightAngleEqual]);
        test_lex(">> =", &[Tag::RightAngleRightAngle, Tag::Equal]);
    }

    #[test]
    fn keywords() {
        test_lex("use", &[Tag::Use]);
        test_lex("fn", &[Tag::Fn]);
        test_lex("return", &[Tag::Return]);
        test_lex("let", &[Tag::Let]);
        test_lex("mut", &[Tag::Mut]);
        test_lex("type", &[Tag::Type]);
        test_lex("if", &[Tag::If]);
        test_lex("else", &[Tag::Else]);
        test_lex("yield", &[Tag::Yield]);
        test_lex("struct", &[Tag::Struct]);
        test_lex("enum", &[Tag::Enum]);
        test_lex("variant", &[Tag::Variant]);
        test_lex("defer", &[Tag::Defer]);
        test_lex("for", &[Tag::For]);
        test_lex("break", &[Tag::Break]);
        test_lex("or", &[Tag::Or]);
        test_lex("and", &[Tag::And]);
        test_lex("not", &[Tag::Not]);
        test_lex("true", &[Tag::True]);
        test_lex("false", &[Tag::False]);
    }

    #[test]
    fn line_comments() {
        test_lex("// test", &[]);
        test_lex("return // test\nlet", &[Tag::Return, Tag::Let]);
    }

    #[test]
    fn arith_fm() {
        test_lex(
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/resources/test/", "arith.fm")),
            &[
                Tag::Let, Tag::Ident, Tag::Equal, Tag::Fn, Tag::LeftParen, Tag::RightParen, Tag::Ident, Tag::LeftBrace,
                Tag::Let, Tag::Ident, Tag::Equal, Tag::IntLit, Tag::Semi,
                Tag::Let, Tag::Ident, Tag::Equal, Tag::IntLit, Tag::Semi,
                Tag::Let, Tag::Ident, Tag::Equal, Tag::Ident, Tag::Plus, Tag::Ident, Tag::Semi,
                Tag::RightBrace, Tag::Semi,
            ]
        );
    }

    #[test]
    fn fact_iter_fm() {
        test_lex(
            include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/resources/test/", "fact-iter.fm")),
            &[
                Tag::Let, Tag::Ident, Tag::Equal, Tag::Fn, Tag::LeftParen, Tag::Ident, Tag::Colon, Tag::Ident, Tag::RightParen, Tag::Ident, Tag::LeftBrace,
                Tag::Let, Tag::Mut, Tag::Ident, Tag::Colon, Tag::Ident, Tag::Equal, Tag::IntLit, Tag::Semi,
                Tag::For,
                Tag::Let, Tag::Mut, Tag::Ident, Tag::Equal, Tag::IntLit, Tag::Semi,
                Tag::Ident, Tag::LeftAngleEqual, Tag::Ident, Tag::Semi,
                Tag::Ident, Tag::PlusEqual, Tag::IntLit,
                Tag::LeftBrace,
                Tag::Ident, Tag::AsteriskEqual, Tag::Ident, Tag::Semi,
                Tag::RightBrace,
                Tag::Return, Tag::Ident, Tag::Semi,
                Tag::RightBrace,
            ]
        );
    }
}
