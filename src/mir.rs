mod scope;
mod reduction;
mod integer_literal;
mod scope2;

pub mod inst {
    use pack_derive::ExtraData;

    use crate::{util::extra, ast::node};

    #[derive(Debug)]
    pub struct Inst {
        pub tag: Tag,
        pub data: Data,
    }

    #[derive(Debug)]
    pub enum Tag {
        // load an integer immediate
        // data.int = int value
        Int,
        // load a float immediate
        // data.float = float value
        Float,

        // binary arithmetic operations
        // data.pl_node.pl = Inst.Binary
        Add,
        Sub,
        Mul,
        Div,
        Mod,

        // binary comparison operations
        // data.pl_node.pl = Inst.Binary
        CmpEq,
        CmpNe,
        CmpGt,
        CmpGe,
        CmpLt,
        CmpLe,

        Lsl,
        Lsr,
        Asl,
        Asr,

        // high-level coercion between types
        // expands to safe implicit cast (most instructions),
        // integer immediate copy (for immediates), or noop (already destination type)
        // data.pl_node.pl = Inst.Coerce
        Coerce,

        Push,
        Load,
        Store,

        FnDecl,
        Call,

        Block,

        BranchSingle,
        BranchDouble,
        Loop,
        Break,

        RetNode,
        RetImplicit,

        YieldNode,
        YieldImplicit,

        Module,
    }

    #[derive(Debug)]
    pub enum Data {
        // integer immediate
        Int(u64),
        // float immediate
        Float(f64),
        // binary operation
        Bin { l: Ref, r: Ref },
        PlNode {
            // reference to the node creating this instruction
            node: node::Index,
            // index into extra where payload is stored
            pl: extra::Index,
        }
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Ref {
        U8,
        U16,
        U32,
        U64,
        I8,
        I16,
        I32,
        I64,
        F32,
        F64,
        Bool,
        Void,

        Zero,
        One,
        True,
        False,
        None,

        Index(u32),
    }

    #[derive(Debug, Clone, Copy)]
    pub struct Link(u32);

    impl From<u32> for Link {
        fn from(value: u32) -> Self {
            Link(value)
        }
    }

    impl From<Index> for Link {
        fn from(value: Index) -> Self {
            Link(value.0)
        }
    }

    impl Into<extra::Data> for Link {
        fn into(self) -> extra::Data {
            extra::Data::from(self.0)
        }
    }

    impl From<usize> for Link {
        fn from(value: usize) -> Self {
            assert!(value <= (u32::MAX as usize));
            Link(value as u32)
        }
    }

    // TODO: check the performance of this, it may be horrible if the matches
    // don't optimize into LUTs, and everyone will be sad
    impl From<Link> for Ref {
        fn from(value: Link) -> Self {
            let ref_len = (std::mem::variant_count::<Ref>() as u32) - 1;

            match value.0 {
                0 => Ref::U8,
                1 => Ref::U16,
                2 => Ref::U32,
                3 => Ref::U64,
                4 => Ref::I8,
                5 => Ref::I16,
                6 => Ref::I32,
                7 => Ref::I64,
                8 => Ref::F32,
                9 => Ref::F64,
                10 => Ref::Bool,
                11 => Ref::Void,
                12 => Ref::Zero,
                13 => Ref::One,
                14 => Ref::True,
                15 => Ref::False,
                16 => Ref::None,
                _ => {
                    Ref::Index(value.0 - ref_len)
                },
            }
        }
    }

    impl From<Ref> for Link {
        fn from(value: Ref) -> Link {
            let ref_len = (std::mem::variant_count::<Ref>() as u32) - 1;

            let l = match value {
                Ref::U8 => 0,
                Ref::U16 => 1,
                Ref::U32 => 2,
                Ref::U64 => 3,
                Ref::I8 => 4,
                Ref::I16 => 5,
                Ref::I32 => 6,
                Ref::I64 => 7,
                Ref::F32 => 8,
                Ref::F64 => 9,
                Ref::Bool => 10,
                Ref::Void => 11,
                Ref::Zero => 12,
                Ref::One => 13,
                Ref::True => 14,
                Ref::False => 15,
                Ref::None => 16,
                Ref::Index(index) => index + ref_len,
            };

            Link(l)
        }
    }

    #[derive(Clone, Copy, Debug)]
    pub struct Index(u32);

    impl From<usize> for Index {
        fn from(value: usize) -> Index {
            assert!(value <= (u32::MAX as usize));
            Index(value as u32)
        }
    }

    impl From<Index> for u32 {
        fn from(value: Index) -> u32 {
            value.0
        }
    }

    impl std::ops::Add<u32> for Index {
        type Output = Index;

        fn add(self, rhs: u32) -> Index {
            Index(self.0 + rhs)
        }
    }

    // pub struct Slice<'a> (&'a [Node]);

    // impl <'a> std::ops::Index<Index> for Slice<'a> {
    //     type Output = Node;

    //     fn index(&self, index: Index) -> &'a Node {
    //         &self.0[index.value()]
    //     }
    // }

    // impl <'a> Slice<'a> {
    //     pub fn from(slice: &'a [Node]) -> Slice {
    //         Slice(slice)
    //     }
    // }
    #[derive(ExtraData)]
    pub struct Binary {
        pub lref: Link,
        pub rref: Link,
    }
}

#[cfg(test)]
mod tests {
    use super::inst::{Ref, Link};

    fn test_ref(r: Ref) {
        let l: Link = r.into();
        assert_eq!(r, Ref::from(l))
    }

    #[test]
    fn ref_from_into() {
        test_ref(Ref::U8);
        test_ref(Ref::U16);
        test_ref(Ref::U32);
        test_ref(Ref::U64);
        test_ref(Ref::I8);
        test_ref(Ref::I16);
        test_ref(Ref::I32);
        test_ref(Ref::I64);
        test_ref(Ref::F32);
        test_ref(Ref::F64);
        test_ref(Ref::Bool);
        test_ref(Ref::Void);
        test_ref(Ref::Zero);
        test_ref(Ref::One);
        test_ref(Ref::True);
        test_ref(Ref::False);
        test_ref(Ref::None);
        test_ref(Ref::Index(0));
        test_ref(Ref::Index(1));
        test_ref(Ref::Index(5));
        test_ref(Ref::Index(42));
    }
}
