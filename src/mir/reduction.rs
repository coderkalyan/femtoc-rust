// reduces the ast to the first pass of MIR

use crate::{ast::{node, Ast}, util::{interner::Interner, extra}};

use super::{scope2::Block, inst::{Index, Inst, Tag, Data, Link, Ref, self}, integer_literal::{parse_int, ParseError}};

#[derive(Debug)]
pub struct Reduction<'a> {
    pub tree: &'a Ast<'a>,
    pub instructions: Vec<Inst>,
    pub interner: Interner,
    extra: extra::Vec,
}

impl <'a> Reduction<'_> {
    fn add_extra<T: extra::ExtraData<T>>(&mut self, data: T) -> extra::Index {
        let len = self.extra.len();
        assert!(len <= (u32::MAX as usize) - std::mem::size_of::<T>());
        data.pack(&mut self.extra);
        extra::Index::from(len)
    }
}

fn add_int(b: &mut Block, int: u64) -> Index {
    b.add_inst(Inst {
        tag: Tag::Int,
        data: Data::Int(int),
    })
}

fn add_float(b: &mut Block, float: f64) -> Index {
    b.add_inst(Inst {
        tag: Tag::Float,
        data: Data::Float(float),
    })
}

fn add_binary(b: &mut Block, l: Link, r: Link, tag: inst::Tag, node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::Binary {
        lref: l,
        rref: r,
    });

    b.add_inst(Inst {
        tag,
        data: inst::Data::PlNode { pl, node },
    })
}

fn integer_literal(b: &mut Block, node: node::Index) -> Result<Link, ParseError> {
    let int_token = b.red.tree.main_token(node);
    let int_str = b.red.tree.token_string(int_token);
    let int = parse_int(int_str)?;

    Ok(match int {
        0 => Ref::Zero.into(),
        1 => Ref::One.into(),
        _ => Link::from(add_int(b, int)),
    })
}

// fn float_literal(b: &mut Block, node: node::Index) -> Link {
//     let float_token = b.red.tree.main_token(node);
//     let float_str = b.red.tree.token_string(float_token);
//     let float = parse_float(float_str);

//     Link::from(add_int(b, float))
// }
