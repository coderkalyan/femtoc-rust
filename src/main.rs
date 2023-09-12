#![feature(variant_count)]

mod lex;
mod ast;
mod util;
mod errors;
mod fmt;
mod mir;

use std::fs;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    filename: String,
}

fn main() {
    let args = Args::parse();
    let data = fs::read_to_string(args.filename).unwrap();
    let ast = ast::parse(&data);
    let module = mir::reduce(&ast);
    println!("{:?}", module);
}
