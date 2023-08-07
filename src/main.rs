mod lex;
mod ast;
mod parse;
pub mod extra;
use std::fs;

use ast::node::{self, FnSignature};

fn main() {
    // let signature = FnSignature {
    //     params_start: extra::Index::from(1),
    //     params_end: extra::Index::from(2),
    //     return_type: node::Index::from(1),
    // };

    // let mut vec = Vec::new();
    // signature.pack(&mut vec);
    // let unpacked = FnSignature::unpack(&vec, 0);

    // println!("packed: {:?}", vec);
    // println!("unpacked: {:?}", unpacked);
    let data = fs::read_to_string("resources/spec/loops.fm").unwrap();
    parse::parse(&data);
}
