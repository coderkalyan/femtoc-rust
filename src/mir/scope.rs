use std::{collections::HashMap, cell::RefCell};

use crate::{ast::node, util::extra};

use super::{reduction::Reduction, inst::{Inst, Index, Link}};

#[derive(Debug, PartialEq)]
pub enum ResolveError {
    IdentifierShadowed,
    IdentifierNotFound,
}

#[derive(Debug)]
pub struct Base<'a, 'bump> {
    pub parent: Option<&'a Base<'a, 'bump>>,
    pub scope: Scope<'a, 'bump>,
}

#[derive(Debug)]
pub enum Scope<'a, 'bump> {
    Module(Module),
    Namespace(Namespace),
    Block(Block<'a, 'bump>),
    Body(Body),
    LocalVal(LocalVal),
    LocalPtr(LocalPtr),
    LocalType(LocalType),
}

#[derive(Debug)]
pub struct Module {
    decls: RefCell<HashMap<u32, u32>>,
    types: RefCell<HashMap<u32, u32>>,
}

#[derive(Debug)]
pub struct Namespace {
    decls: RefCell<HashMap<u32, node::Index>>,
    types: RefCell<HashMap<u32, node::Index>>,
}

#[derive(Debug)]
pub struct Block<'a, 'bump> {
    instructions: Vec<u32>,
    scratch: bumpalo::collections::Vec<'bump, extra::Data>,
    pub red: &'a mut Reduction<'a>,
}

#[derive(Debug)]
pub struct Body {
    fn_node: node::Index,
}

#[derive(Debug)]
pub struct LocalVal {
    ident: u32,
    link: Link,
}

#[derive(Debug)]
pub struct LocalPtr {
    ident: u32,
    link: Link,
}

#[derive(Debug)]
pub struct LocalType {
    ident: u32,
    link: Link,
}

impl Module {
    pub fn new() -> Module {
        Module {
            decls: RefCell::new(HashMap::new()),
            types: RefCell::new(HashMap::new()),
        }
    }

    pub fn insert_decl(&self, ident: u32, node: node::Index) {
        self.decls.borrow_mut().insert(ident, node);
    }
}

impl Namespace {
    pub fn new() -> Namespace {
        Namespace {
            decls: RefCell::new(HashMap::new()),
            types: RefCell::new(HashMap::new()),
        }
    }

    pub fn insert_decl(&self, ident: u32, node: node::Index) {
        self.decls.borrow_mut().insert(ident, node);
    }
}

impl <'a, 'bump> Block<'_, '_> {
    pub fn add_inst(&mut self, inst: Inst) -> Index {
        // reserve in both before adding atomically
        let index = Index::from(self.red.instructions.len());
        self.instructions.reserve(1);
        self.instructions.reserve(1);

        self.red.instructions.push(inst);
        self.instructions.push(index.into());

        index
    }
}

impl LocalVal {
    pub fn new(ident: u32, link: Link) -> LocalVal {
        LocalVal { ident, link }
    }
}

impl LocalPtr {
    pub fn new(ident: u32, link: Link) -> LocalPtr {
        LocalPtr { ident, link }
    }
}

impl LocalType {
    pub fn new(ident: u32, link: Link) -> LocalType {
        LocalType { ident, link }
    }
}

impl <'a, 'bump> From<Namespace> for Scope<'a, 'bump> {
    fn from(value: Namespace) -> Self {
        Scope::Namespace(value)
    }
}

impl <'a, 'bump> From<Block<'a, 'bump>> for Scope<'a, 'bump> {
    fn from(value: Block<'a, 'bump>) -> Self {
        Scope::Block(value)
    }
}

impl <'a, 'bump> From<LocalVal> for Scope<'a, 'bump> {
    fn from(value: LocalVal) -> Self {
        Scope::LocalVal(value)
    }
}

impl <'a, 'bump> From<LocalPtr> for Scope<'a, 'bump> {
    fn from(value: LocalPtr) -> Self {
        Scope::LocalPtr(value)
    }
}

impl <'a, 'bump> From<LocalType> for Scope<'a, 'bump> {
    fn from(value: LocalType) -> Self {
        Scope::LocalType(value)
    }
}

impl <'a, 'b> Base<'a, 'b> {
    pub fn resolve_var(&'a self, ident: u32) -> Result<&'a Base<'a, 'b>, ResolveError> {
        let mut found: Option<&'a Base> = None;
        let mut s = self;

        loop {
            match s.scope {
                Scope::Module(ref module) => {
                    match module.decls.borrow().get(&ident) {
                        Some(_) => match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        },
                        None => {},
                    }
                    break;
                },
                Scope::Namespace(ref namespace) => {
                    match namespace.decls.borrow().get(&ident) {
                        Some(_) => match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        },
                        None => {},
                    }
                },
                Scope::Block(_) | Scope::Body(_) => {},
                Scope::LocalVal(ref local_val) => {
                    if local_val.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }
                },
                Scope::LocalPtr(ref local_ptr) => {
                    if local_ptr.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }
                },
                Scope::LocalType(_) => {},
            }

            s = s.parent.unwrap();
        }

        match found {
            Some(scope) => Ok(scope),
            None => Err(ResolveError::IdentifierNotFound),
        }
    }

    pub fn resolve_type(&'a self, ident: u32) -> Result<&'a Base<'a, 'b>, ResolveError> {
        let mut found: Option<&'a Base> = None;
        let mut s = self;

        loop {
            match s.scope {
                Scope::Module(ref module) => {
                    match module.types.borrow().get(&ident) {
                        Some(_) => match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        },
                        None => {},
                    }
                    break;
                },
                Scope::Namespace(ref namespace) => {
                    match namespace.decls.borrow().get(&ident) {
                        Some(_) => match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        },
                        None => {},
                    }
                },
                Scope::Block(_) | Scope::Body(_) => {},
                Scope::LocalVal(_) | Scope::LocalPtr(_) => {},
                Scope::LocalType(ref local_val) => {
                    if local_val.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }
                },
            }

            s = s.parent.unwrap();
        }

        match found {
            Some(scope) => Ok(scope),
            None => Err(ResolveError::IdentifierNotFound),
        }
    }
}

#[cfg(test)]
mod test {
    use std::{collections::HashMap, cell::RefCell};

    use crate::{util::interner::Interner, ast::node, mir::inst::Link};

    use super::{Scope, Base, Namespace, ResolveError, LocalVal, LocalPtr};

    fn compare_resolve<'a, 'b>(a: Result<&'a Base<'a, 'b>, ResolveError>, b: Result<&'a Base<'a, 'b>, ResolveError>) {
        match b {
            Ok(scope) => assert_eq!(a.unwrap() as *const _, scope as *const _),
            Err(e) => assert_eq!(a.unwrap_err(), e),
        }
    }

    #[test]
    fn namespace_member() {
        let mut interner = Interner::new();

        let module = Base {
            parent: None,
            scope: Scope::Module,
        };
        let namespace = Base {
            parent: Some(&module),
            scope: Namespace::new().into(),
        };

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));

        compare_resolve(namespace.resolve_var(apple), Err(ResolveError::IdentifierNotFound));
        compare_resolve(namespace.resolve_var(banana), Err(ResolveError::IdentifierNotFound));
        compare_resolve(namespace.resolve_var(cherry), Err(ResolveError::IdentifierNotFound));

        if let Scope::Namespace(ref ns) = namespace.scope {
            ns.insert_decl(apple, 0u32.into());
            ns.insert_decl(banana, 1u32.into());
            ns.insert_decl(cherry, 2u32.into());
        }

        compare_resolve(namespace.resolve_var(apple), Ok(&namespace));
        compare_resolve(namespace.resolve_var(banana), Ok(&namespace));
        compare_resolve(namespace.resolve_var(cherry), Ok(&namespace));
    }

    #[test]
    fn local_var() {
        let mut interner = Interner::new();

        let module = Base {
            parent: None,
            scope: Scope::Module,
        };
        let namespace = Base {
            parent: Some(&module),
            scope: Namespace::new().into(),
        };

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));

        let a = node::Index::from(0u32);
        let b = Link::from(1u32);
        let c = Link::from(2u32);

        if let Scope::Namespace(ref ns) = namespace.scope {
            ns.insert_decl(apple, a);
        }

        let banana_var = Base {
            parent: Some(&namespace),
            scope: LocalVal::new(banana, b).into(),
        };
        let cherry_var = Base {
            parent: Some(&banana_var),
            scope: LocalPtr::new(cherry, c).into(),
        };

        compare_resolve(namespace.resolve_var(apple), Ok(&namespace));
        compare_resolve(namespace.resolve_var(banana), Err(ResolveError::IdentifierNotFound));
        compare_resolve(namespace.resolve_var(cherry), Err(ResolveError::IdentifierNotFound));

        compare_resolve(banana_var.resolve_var(apple), Ok(&namespace));
        compare_resolve(banana_var.resolve_var(banana), Ok(&banana_var));
        compare_resolve(banana_var.resolve_var(cherry), Err(ResolveError::IdentifierNotFound));

        compare_resolve(cherry_var.resolve_var(apple), Ok(&namespace));
        compare_resolve(cherry_var.resolve_var(banana), Ok(&banana_var));
        compare_resolve(cherry_var.resolve_var(cherry), Ok(&cherry_var));
    }

    #[test]
    fn namespace_shadowing() {
        let mut interner = Interner::new();

        let module = Base {
            parent: None,
            scope: Scope::Module,
        };
        let namespace = Base {
            parent: Some(&module),
            scope: Namespace::new().into(),
        };

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));

        let c = Link::from(2 as u32);
        let d = Link::from(3 as u32);

        if let Scope::Namespace(ref ns) = namespace.scope {
            ns.insert_decl(apple, 0u32.into());
            ns.insert_decl(banana, 1u32.into());
        }

        let cherry_var = Base {
            parent: Some(&namespace),
            scope: LocalVal::new(cherry, c).into(),
        };
        let illegal_var = Base {
            parent: Some(&cherry_var),
            scope: LocalVal::new(apple, d).into(),
        };

        compare_resolve(cherry_var.resolve_var(apple), Ok(&namespace));
        compare_resolve(cherry_var.resolve_var(banana), Ok(&namespace));
        compare_resolve(cherry_var.resolve_var(cherry), Ok(&cherry_var));

        compare_resolve(illegal_var.resolve_var(apple), Err(ResolveError::IdentifierShadowed));
    }

    // fn block_shadowing() {
    //     let mut interner = Interner::new();

    //     let module = Base {
    //         parent: None,
    //         scope: Scope::Module,
    //     };
    //     // let outer = Scope::new_block

    //     let apple = interner.intern(String::from("apple"));
    //     let banana = interner.intern(String::from("banana"));
    //     let cherry = interner.intern(String::from("cherry"));

    //     let a = Link::from(0);
    //     let b = Link::from(1);
    //     let c = Link::from(2);
    //     let d = Link::from(3);
    //     let e = Link::from(4);

    //     let apple_var = Scope::new_local_val(&outer, apple, a);
    //     let banana_var = Scope::new_local_val(&apple_var, banana, b);
    // }
}
