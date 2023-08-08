extern crate pack_derive;

mod parser;

use bumpalo::Bump;
use crate::lex::{token, Lexer};
use crate::util::extra;
use crate::ast::parser::Parser;
// represents the entire, immutable AST of a source file, once parsed
// in-progress mutable parsing data is stored in the `Parser` struct in ast/parser.rs
// the AST owns the source, token list, node list, and node extra_data list
// #[derive(Clone)]
pub struct Ast<'a> {
    source: &'a str,
    tokens: Vec<token::CompactToken>,
    nodes: Vec<node::Node>,
    extra: extra::Vec,
    // nodes: node::Slice<'a>,
    // extra: extra::Slice<'a>,
    // errors: &'a [SourceError],
}

// pub struct ByteOffset(u32);

pub mod node {
    use pack_derive::ExtraData;

    use crate::lex::token;
    use crate::util::extra;

    // we store an array of token tags and start locations
    // to reference during parsing. AST nodes don't store tokens
    // or strings themselves, but rather the index to the relevant token(s)
    // in this array. note that tokens themselves don't own 
    // any strings, but reference character offsets in the global
    // source array.
    // type TokenList = Vec<struct {  }
    // represents a node in the femto abstract syntax tree
    // due to the u32::max cap on source file size, we can
    // also cap the AST at u32::max nodes. hence, nodes are
    // inserted into a flat, contigious arraylist. node pointers
    // use u32 indices instead of pointer types. this reduces
    // memory footprint and improves cache locality
    //
    // the AST node is a binary tree because the vast majority
    // of nodes only need two children (or fewer) and adding more
    // would be a waste. nodes that need to reference more children
    // use the extra_data array explained below in the `Ast` struct
    #[derive(Debug)]
    pub struct Node {
        // index to the "main" token representing this node, if applicable.
        // examples include 'fn' for function declarations, 'let' for
        // const or var declarations, and operator tokens for unary and
        // binary operations. strings such as identifiers can be extracted
        // from this information using a fixed offset (i.e. +1 for const decls)
        pub main_token: token::Index,

        // up to two u32 child references
        pub data: Data,
    }

    // each union member can hold up to
    // two u32 child references. these are either
    // `Index` types to index into the nodes array
    // or `extra::Index` to index into the extra_data array
    //
    // extra data indices represent only the "start" of the
    // unpacked extra data struct in the extra_data array
    #[derive(Debug, Clone)]
    pub enum Data {
        NamedType,
        // function declaration 'fn (params...) ret { body }'
        // main_token = n/a
        // proto = FnSignature {} 'fn (params...) ret'
        // body = body block node
        FnDecl { signature: extra::Index, body: Index },
        // function parameter (in the declaration/prototype)
        // as opposed to *arguments* at the call site
        // 'argc: u32'
        // main_token = name identifier
        // ty = parameter type node
        Param { ty: Index },

        // expressions
        // integer literal '123_456'
        // main_token = unparsed literal string
        IntegerLiteral,
        // float literal '1.2345'
        // main_token = unparsed literal string
        FloatLiteral,
        // boolean literal 'true'
        BoolLiteral,
        // binary expression 'a [+-*/...] b'
        // main_token = operator_token
        // left = left side expression node
        // right = right side expression node
        BinaryExpr { left: Index, right: Index },
        // variable value 'x'
        // main_token = variable identifier
        VarExpr,
        // function call 'foo(1, 2, 3)'
        // main_token = function value being called
        // args_start = start of argument array
        // args_end = end of argument array
        CallExpr { args_start: extra::Index, args_end: extra::Index },
        // string literal '"Hello, world!"'
        // main_token = string literal
        StringLiteral,

        // declarations
        // type alias 'type Index = u32'
        // main_token = 'type'
        // ty = type name node
        TypeDecl { ty: Index },

        // statements
        // block '{...}'
        // main_token = '{'
        // stmts_start = start of statement array
        // stmts_end = end of statement array
        Block { stmts_start: extra::Index, stmts_end: extra::Index },
        // constant declaration 'let x[: ty] = 1'
        // main_token = 'let'
        // ty = type node
        // val = value node
        ConstDecl { ty: Index, val: Index },
        // var declaration 'let mut x[: ty] = 1'
        // main_token = 'let'
        // ty = type node
        // val = value node
        VarDecl { ty: Index, val: Index },

        // variable assignment 'foo = "bar"'
        // main_token = variable name
        // val = value node
        AssignSimple { val: Index },
        // variable assignment with operator 'foo += 1'
        // main_token = variable name
        AssignBinary { val: Index },

        // return value 'return 5'
        // main_token = 'return'
        // val = return value node
        ReturnVal { val: Index },

        // simple if statement 'if cond { body }'
        // main_token = 'if'
        // condition = conditional expression node
        // exec_true = block node to execute on true
        IfSimple { condition: Index, exec_true: Index },

        // if else statement 'if cond {} else {}'
        // main_token = 'if'
        // condition = conditional expression node
        // exec = extra index to body blocks
        IfElse { condition: Index, exec: extra::Index },

        // chained if-else if statement 'if cond {} else if cond {}'
        // main_token = 'if'
        // condition = conditional expression node
        // chain = extra index to chain information
        IfChain { condition: Index, chain: extra::Index },

        // forever loop 'for {}'
        // main_token = 'for'
        // body = body block node
        LoopForever { body: Index },

        // loop while condition satisfied 'for cond {}'
        // main_token = 'for'
        // condition = conditional expression node
        // body = body block node
        LoopConditional { condition: Index, body: Index },

        // traditional range loop while condition satisfied
        // with binding and afterthought 'for let mut i: u32 = 0; i < 10; i += 1 {}'
        // main_token = 'for'
        // signature = extra index to loop range signature
        LoopRange { signature: extra::Index, body: Index },

        // 'break' statement to exit a loop early
        // main_token = 'break'
        Break,

        Module { stmts_start: extra::Index, stmts_end: extra::Index },
    }

    #[derive(Clone, Copy, Debug)]
    pub struct Index(u32);

    impl Index {
        pub fn from(index: usize) -> Index {
            assert!(index <= (u32::MAX as usize));
            Index(index as u32)
        }

        pub fn value(&self) -> usize {
            self.0 as usize
        }

        pub fn to_extra_data(&self) -> extra::Data {
            extra::Data::from(self.0)
        }
    }

    impl std::ops::Add<u32> for Index {
        type Output = Index;

        fn add(self, rhs: u32) -> Index {
            Index(self.0 + rhs)
        }
    }

    pub struct Slice<'a> (&'a [Node]);

    impl <'a> std::ops::Index<Index> for Slice<'a> {
        type Output = Node;

        fn index(&self, index: Index) -> &'a Node {
            &self.0[index.value()]
        }
    }

    impl <'a> Slice<'a> {
        pub fn from(slice: &'a [Node]) -> Slice {
            Slice(slice)
        }
    }

    // function signature, excluding return type
    // params_start = start of parameter node index array
    // params_end = end of parameter node index array
    #[derive(ExtraData, Debug)]
    pub struct FnSignature {
        pub params_start: extra::Index,
        pub params_end: extra::Index,
        pub return_type: Index,
    }

    // if else execution information
    // exec_true = block node to execute if condition is met
    // exec_false = block node to execute if condition is not met
    #[derive(ExtraData, Debug)]
    pub struct IfElse {
        pub exec_true: Index,
        pub exec_false: Index,
    }

    // chained if else if execution information
    // exec_true = block node to execute if condition is met
    // next = next conditional node in the chain (if, if else, or another if chain)
    #[derive(ExtraData, Debug)]
    pub struct IfChain {
        pub exec_true: Index,
        pub next: Index,
    }

    // ranged-based for loop signature
    // binding = variable declaration at beginning of loop
    // condition = boolean expression to check loop continuation
    // afterthought = statement executed at the end of each loop iteration
    #[derive(ExtraData, Debug)]
    pub struct RangeSignature {
        pub binding: Index,
        pub condition: Index,
        pub afterthought: Index,
    }
}


impl <'a> Ast<'_> {
    pub fn token_string(&'a self, index: token::Index) -> &'a str {
        let slice = token::CompactSlice::from(&self.tokens);
        let token_start = slice[index].start;
        let mut lexer = Lexer::new_index(&self.source, token_start);
        let token = lexer.next();
        let start = token.loc.start as usize;
        let end = token.loc.end as usize;

        return &(&self.source)[start..end];
    }

    pub fn token_tag(&self, index: token::Index) -> token::Tag {
        let slice = token::CompactSlice::from(&self.tokens);
        slice[index].tag
    }

    pub fn main_token(&self, node: node::Index) -> token::Index {
        let slice = node::Slice::from(&self.nodes);
        slice[node].main_token
    }

    pub fn data(&self, node: node::Index) -> node::Data {
        let slice = node::Slice::from(&self.nodes);
        slice[node].data.clone()
    }
}

pub fn parse(source: &str) -> Ast {
    let mut tokens = Vec::<token::CompactToken>::new();
    let mut lexer = Lexer::new(source);
    loop {
        let token = lexer.next();
        tokens.push(token::CompactToken {
            tag: token.tag,
            start: token.loc.start,
        });
        match token.tag {
            token::Tag::Eof => break,
            _ => {},
        }
    }

    // TODO: is moving tuples like this good?
    // we want to run the destructor for Parser (actually non-existant)
    // before we move parser.nodes and parser.extra out of the function
    let (nodes, extra) = {
        let bump = Bump::new();
        let tokens_slice = token::CompactSlice::from(&tokens);
        let mut parser = Parser::new(&bump, source, tokens_slice);
        let _ = parser.parse_module().unwrap(); // TODO: error handling

        (parser.nodes, parser.extra)
    };

    Ast {
        source,
        tokens,
        nodes,
        extra,
    }
}
