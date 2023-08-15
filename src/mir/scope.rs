use std::collections::HashMap;
use bumpalo::Bump;

use crate::{ast::node, util::extra};

use super::{inst::Link, reduction::Reduction};

#[derive(Debug, PartialEq)]
pub enum ResolveError {
    IdentifierShadowed,
    IdentifierNotFound,
}

#[derive(Debug)]
pub struct Namespace<'a> {
    parent: &'a Scope<'a>,
    decls: HashMap<u32, node::Index>,
    types: HashMap<u32, node::Index>,
}

#[derive(Debug)]
pub struct Block<'a> {
    parent: &'a Scope<'a>,
    instructions: Vec<u32>,
    scratch: bumpalo::collections::Vec<'a, extra::Data>,
    red: &'a mut Reduction,
}

#[derive(Debug)]
pub struct Body<'a> {
    parent: &'a Scope<'a>,
    fn_node: node::Index,
}

#[derive(Debug)]
pub struct LocalVal<'a> {
    parent: &'a Scope<'a>,
    ident: u32,
    link: Link,
}

#[derive(Debug)]
pub struct LocalPtr<'a> {
    parent: &'a Scope<'a>,
    ident: u32,
    link: Link,
}

#[derive(Debug)]
pub struct LocalType<'a> {
    parent: &'a Scope<'a>,
    ident: u32,
    link: Link,
}

#[derive(Debug)]
pub enum Scope<'a> {
    Module,
    Namespace(Namespace<'a>),
    Block(Block<'a>),
    Body(Body<'a>),
    LocalVal(LocalVal<'a>),
    LocalPtr(LocalPtr<'a>),
    LocalType(LocalType<'a>),
}

impl <'a> Scope<'a> {
    pub fn new_module() -> Scope<'a> { Scope::Module }

    pub fn new_namespace(parent: &'a Scope) -> Scope<'a> {
        Scope::Namespace(Namespace {
            parent,
            decls: HashMap::new(),
            types: HashMap::new() ,
        })
        // TODO: types bug in resolve_type
    }

    pub fn new_block(parent: &'a Scope, bump: &'a Bump, red: &'a mut Reduction) -> Scope<'a> {
        Scope::Block(Block {
            parent,
            instructions: Vec::new(),
            scratch: bumpalo::collections::Vec::new_in(bump),
            red,
        })
    }

    pub fn new_body(parent: &'a Scope, fn_node: node::Index) -> Scope<'a> {
        Scope::Body(Body { parent, fn_node })
    }

    pub fn new_local_val(parent: &'a Scope, ident: u32, link: Link) -> Scope<'a> {
        Scope::LocalVal(LocalVal { parent, ident, link })
    }

    pub fn new_local_ptr(parent: &'a Scope, ident: u32, link: Link) -> Scope<'a> {
        Scope::LocalPtr(LocalPtr { parent, ident, link })
    }

    pub fn new_local_type(parent: &'a Scope, ident: u32, link: Link) -> Scope<'a> {
        Scope::LocalType(LocalType { parent, ident, link })
    }

    pub fn resolve_var(&'a self, ident: u32) -> Result<&'a Scope<'a>, ResolveError> {
        let mut found: Option<&'a Scope> = None;
        let mut s: &'a Scope<'a> = self;

        loop {
            match s {
                Scope::Module => break,
                Scope::Namespace(namespace) => {
                    match namespace.decls.get(&ident) {
                        Some(_) => match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        },
                        None => {},
                    }

                    s = namespace.parent;
                },
                Scope::Block(block) => {
                    s = block.parent;
                },
                Self::Body(body) => s = body.parent,
                Self::LocalVal(local_val) => {
                    if local_val.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }

                    s = local_val.parent;
                },
                Self::LocalPtr(local_ptr) => {
                    if local_ptr.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }

                    s = local_ptr.parent;
                },
                Self::LocalType(local_type) => s = local_type.parent,
            }
        }

        match found {
            Some(scope) => Ok(scope),
            None => Err(ResolveError::IdentifierNotFound),
        }
    }

    pub fn resolve_type(&'a self, ident: u32) -> Result<&'a Scope<'a>, ResolveError> {
        let mut found: Option<&'a Scope> = None;
        let mut s: &'a Scope<'a> = self;

        loop {
            match s {
                Scope::Module => break,
                Scope::Namespace(namespace) => {
                    match namespace.decls.get(&ident) {
                        Some(_) => match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        },
                        None => {},
                    }

                    s = namespace.parent;
                },
                Scope::Block(block) => {
                    s = block.parent;
                },
                Self::Body(body) => s = body.parent,
                Self::LocalVal(local_val) => {
                    if local_val.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }

                    s = local_val.parent;
                },
                Self::LocalPtr(local_ptr) => {
                    if local_ptr.ident == ident {
                        match found {
                            Some(_) => return Err(ResolveError::IdentifierShadowed),
                            None => found = Some(s),
                        }
                    }

                    s = local_ptr.parent;
                },
                Self::LocalType(local_type) => s = local_type.parent,
            }
        }

        match found {
            Some(scope) => Ok(scope),
            None => Err(ResolveError::IdentifierNotFound),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{util::interner::Interner, ast::node, mir::inst::{Ref, Link}};

    use super::{Scope, ResolveError};

    fn compare_resolve(a: Result<&Scope, ResolveError>, b: Result<&Scope, ResolveError>) {
        match b {
            Ok(scope) => assert_eq!(a.unwrap() as *const _, scope as *const _),
            Err(e) => assert_eq!(a.unwrap_err(), e),
        }
    }

    #[test]
    fn namespace_member() {
        let mut interner = Interner::new();
        let module = Scope::new_module();
        let mut namespace = Scope::new_namespace(&module);

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));

        assert_eq!(namespace.resolve_var(apple).unwrap_err(), ResolveError::IdentifierNotFound);
        assert_eq!(namespace.resolve_var(banana).unwrap_err(), ResolveError::IdentifierNotFound);
        assert_eq!(namespace.resolve_var(cherry).unwrap_err(), ResolveError::IdentifierNotFound);

        {
            let decls = match namespace {
                Scope::Namespace(ref mut ns) => &mut ns.decls,
                _ => unreachable!(),
            };
            decls.insert(apple, node::Index::from(0));
            decls.insert(banana, node::Index::from(1));
            decls.insert(cherry, node::Index::from(2));
        }
        
        compare_resolve(namespace.resolve_var(apple), Ok(&namespace));
        compare_resolve(namespace.resolve_var(banana), Ok(&namespace));
        compare_resolve(namespace.resolve_var(cherry), Ok(&namespace));
    }

    #[test]
    fn local_var() {
        let mut interner = Interner::new();
        let module = Scope::new_module();
        let mut namespace = Scope::new_namespace(&module);

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));

        let a = node::Index::from(0);
        let b = Link::from(1);
        let c = Link::from(2);

        {
            let decls = match namespace {
                Scope::Namespace(ref mut ns) => &mut ns.decls,
                _ => unreachable!(),
            };
            decls.insert(apple, a);
        }

        let banana_var = Scope::new_local_val(&namespace, banana, b);
        let cherry_var = Scope::new_local_ptr(&banana_var, cherry, c);

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

    fn namespace_shadowing() {
        let mut interner = Interner::new();
        let module = Scope::new_module();
        let mut namespace = Scope::new_namespace(&module);

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));

        let c = Link::from(2);
        let d = Link::from(3);

        {
            let decls = match namespace {
                Scope::Namespace(ref mut ns) => &mut ns.decls,
                _ => unreachable!(),
            };
            decls.insert(apple, node::Index::from(0));
            decls.insert(banana, node::Index::from(1));
        }

        let cherry_var = Scope::new_local_val(&namespace, cherry, c);
        let illegal_var = Scope::new_local_val(&cherry_var, apple, d);

        compare_resolve(cherry_var.resolve_var(apple), Ok(&namespace));
        compare_resolve(cherry_var.resolve_var(banana), Ok(&namespace));
        compare_resolve(cherry_var.resolve_var(cherry), Ok(&cherry_var));

        compare_resolve(illegal_var.resolve_var(apple), Err(ResolveError::IdentifierShadowed));
    }

    // fn block_shadowing() {
    //     let mut interner = Interner::new();
    //     let module = Scope::new_module();
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
