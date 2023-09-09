// reduces the ast to the first pass of MIR

use crate::{ast::{node, Ast}, util::{interner::Interner, extra}, lex::token};

use super::{scope::{Block, Scope}, inst::{Index, Inst, Tag, Data, Link, Ref, self}, integer_literal::{parse_int, ParseError}};

pub enum MirError {
    ParseError(ParseError),
    UnexpectedToken,
}

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

fn add_block(b: &mut Block, insts: &[Index], node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::Block {
        len: 0u32,
    });
    let insts: Vec<extra::Data> = insts.iter().map(|x| Into::<extra::Data>::into(x.clone())).collect();
    b.red.extra.extend(insts);
    
    b.add_inst(Inst {
        tag: inst::Tag::Block,
        data: inst::Data::PlNode { pl, node },
    })
}

fn add_call(b: &mut Block, addr: Link, args: &[u32], node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::Call {
        addr,
        args_len: args.len() as u32,
    });
    let args: Vec<extra::Data> = args.iter().map(|x| Into::<extra::Data>::into(*x)).collect();
    b.red.extra.extend(args);

    b.add_inst(Inst {
        tag: inst::Tag::Call,
        data: inst::Data::PlNode { pl, node }
    })
}

fn add_param(b: &mut Block, name: u32, ty_link: Link, node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::Param {
        name,
        ty: ty_link,
    });

    b.add_inst(Inst {
        tag: inst::Tag::Param,
        data: inst::Data::PlNode { pl, node },
    })
}

fn add_push(b: &mut Block, val: Link, node: node::Index) -> Index {
    b.add_inst(Inst {
        tag: inst::Tag::Push,
        data: inst::Data::UnaryNode { operand: val, node },
    })
}

fn add_load(b: &mut Block, addr: Index, node: node::Index) -> Index {
    b.add_inst(Inst {
        tag: inst::Tag::Push,
        data: inst::Data::UnaryNode { operand: addr.into(), node },
    })
}

fn add_store(b: &mut Block, addr: Index, val: Link, node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::Store {
        addr,
        val,
    });

    b.add_inst(Inst {
        tag: inst::Tag::Push,
        data: inst::Data::UnaryNode { operand: val, node },
    })
}

fn add_branch_single(b: &mut Block, cond: Link, exec: Index, node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::BranchSingle {
        condition: cond,
        exec_true: exec,
    });

    b.add_inst(Inst {
        tag: inst::Tag::BranchSingle,
        data: inst::Data::PlNode { pl, node }
    })
}

fn add_branch_double(b: &mut Block, cond: Link, exec_true: Index, exec_false: Index, node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::BranchDouble {
        condition: cond,
        exec_true,
        exec_false,
    });

    b.add_inst(Inst {
        tag: inst::Tag::BranchDouble,
        data: inst::Data::PlNode { pl, node }
    })
}

fn add_ret_node(b: &mut Block, val: Link, node: node::Index) -> Index {
    b.add_inst(Inst {
        tag: inst::Tag::RetNode,
        data: inst::Data::UnaryNode { operand: val, node },
    })
}

fn add_ret_implicit(b: &mut Block, val: Link, token: token::Index) -> Index {
    b.add_inst(Inst {
        tag: inst::Tag::RetImplicit,
        data: inst::Data::UnaryToken { operand: val, token },
    })
}

fn add_yield_implicit(b: &mut Block, val: Link, token: token::Index) -> Index {
    b.add_inst(Inst {
        tag: inst::Tag::YieldImplicit,
        data: inst::Data::UnaryToken { operand: val, token },
    })
}

fn add_loop(b: &mut Block, cond: Link, body: Index, node: node::Index) -> Index {
    let pl = b.red.add_extra(inst::Loop {
        condition: cond,
        body,
    });

    b.add_inst(Inst {
        tag: inst::Tag::Loop,
        data: inst::Data::PlNode { pl, node }
    })
}

fn add_break(b: &mut Block, node: node::Index) -> Index {
    b.add_inst(Inst {
        tag: inst::Tag::Break,
        data: inst::Data::Node { node },
    })
}

fn add_module(b: &mut Block, members: &[extra::Data], node: node::Index) -> Index {
    assert_eq!(members.len() % 2, 0);
    let pl = b.red.add_extra(inst::Module {
        len: (members.len() / 2) as u32,
    });
    b.red.extra.extend_from_slice(members);

    b.add_inst(Inst {
        tag: inst::Tag::Module,
        data: inst::Data::PlNode { pl, node },
    })
}

fn integer_literal(b: &mut Block, node: node::Index) -> Result<Link, MirError> {
    let int_token = b.red.tree.main_token(node);
    let int_str = b.red.tree.token_string(int_token);
    let int = match parse_int(int_str) {
        Ok(v) => v,
        Err(e) => return Err(MirError::ParseError(e)),
    };

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

fn bool_literal(b: &mut Block, node: node::Index) -> Link {
    let bool_token = b.red.tree.main_token(node);

    match b.red.tree.token_tag(bool_token) {
        token::Tag::True => Ref::True,
        token::Tag::False => Ref::False,
        _ => unreachable!(),
    }.into()
}

fn binary_raw(b: &mut Block, node: node::Index, op: token::Index, lref: Link, rref: Link) -> Result<Index, MirError> {
    let tag: inst::Tag = match b.red.tree.token_tag(op) {
        token::Tag::Plus => Tag::Add,
        token::Tag::PlusEqual => Tag::Add,
        token::Tag::Minus => Tag::Sub,
        token::Tag::MinusEqual => Tag::Sub,
        token::Tag::Asterisk => Tag::Mul,
        token::Tag::AsteriskEqual => Tag::Mul,
        token::Tag::Slash => Tag::Div,
        token::Tag::SlashEqual => Tag::Div,
        token::Tag::Percent | token::Tag::PercentEqual => Tag::Mod,
        token::Tag::EqualEqual => Tag::CmpEq,
        token::Tag::BangEqual => Tag::CmpNe,
        token::Tag::LeftAngleEqual => Tag::CmpLe,
        token::Tag::RightAngleEqual => Tag::CmpGe,
        token::Tag::LeftAngle => Tag::CmpLt,
        token::Tag::RightAngle => Tag::CmpGt,
        _ => return Err(MirError::UnexpectedToken),
    };

    Ok(add_binary(b, lref, rref, tag, node))
}

fn binary(b: &mut Block, scope: &Scope, node: node::Index) -> Result<Index, MirError> {
    if let node::Data::BinaryExpr { left, right } = b.red.tree.data(node) {
        let operator_token = b.red.tree.main_token(node);
        let left = expr(b, scope, left)?;
        let right = expr(b, scope, right)?;
        binary_raw(b, node, operator_token, left, right)
    } else { 
        unreachable!()
    }
}

fn expr(b: &mut Block, scope: &Scope, node: node::Index) -> Result<Link, MirError> {
    match b.red.tree.data(node) {
        node::Data::IntegerLiteral => integer_literal(b, node),
        node::Data::FloatLiteral => unimplemented!(),
        node::Data::BoolLiteral => Ok(bool_literal(b, node)),
        node::Data::VarDecl { ty, val } => unimplemented!(),
        node::Data::CallExpr { args_start, args_end } => unimplemented!(),
        node::Data::BinaryExpr { left, right } => Ok(binary(b, scope, node)?.into()),
        node::Data::FnDecl { signature, body } => unimplemented!(),
        _ => unimplemented!(),
    }
}
