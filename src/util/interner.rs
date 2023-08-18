use std::collections::HashMap;

// "Interns" strings by storing each unique string exactly once
// and returning a unique handle to the string the next time it
// is interned. Can also return the string given a handle
// TODO: this could be more efficient, partly because the port to rust
// from zig changes some primitives (ArrayHashMap)
#[derive(Debug)]
pub struct Interner {
    handles: HashMap<Box<String>, u32>,
    entries: Vec<*const str>,
}

impl <'a> Interner {
    pub fn new() -> Interner {
        Interner {
            handles: HashMap::new(),
            entries: Vec::new(),
        }
    }

    pub fn intern(&mut self, string: String) -> u32 {
        // if we've seen this string before, return its handle
        match self.handles.get(&string) {
            Some(handle) => return *handle,
            None => {},
        }

        assert!(self.entries.len() < (u32::MAX as usize));
        let handle = self.entries.len() as u32;
        self.handles.insert(Box::from(string.clone()), handle);
        let (ptr, _) = self.handles.get_key_value(&string).unwrap();
        let ptr = ptr.as_str() as *const str;
        self.entries.push(ptr);

        return handle;
    }

    pub fn get(&self, handle: u32) -> Option<&'a str> {
        if (handle as usize) >= self.entries.len() {
            return None;
        }
        let ptr = self.entries[handle as usize];
        unsafe {
            Some(&*ptr)
        }
    }
}

mod test {
    use crate::util::interner::Interner;

    #[test]
    fn interner() {
        let mut interner = Interner::new();
        assert_eq!(interner.handles.len(), 0);
        assert_eq!(interner.entries.len(), 0);

        let apple = interner.intern(String::from("apple"));
        let banana = interner.intern(String::from("banana"));
        let cherry = interner.intern(String::from("cherry"));
        assert_eq!(interner.handles.len(), 3);
        assert_eq!(interner.entries.len(), 3);
        assert!(interner.get(apple).unwrap().eq(&String::from("apple")));
        assert!(interner.get(banana).unwrap().eq(&String::from("banana")));
        assert!(interner.get(cherry).unwrap().eq(&String::from("cherry")));

        // adding the same string again should return the original id
        assert_eq!(apple, interner.intern(String::from("apple")));
        assert_eq!(interner.handles.len(), 3);
        assert_eq!(interner.entries.len(), 3);

        // partly overlapping string is a unique string
        let apfel = interner.intern(String::from("apfel"));
        assert_eq!(interner.handles.len(), 4);
        assert_eq!(interner.entries.len(), 4);
        assert!(interner.get(apfel).unwrap().eq(&String::from("apfel")));
        assert!(apple != apfel);
        assert!(!interner.get(apfel).unwrap().eq(&String::from("apple")));

        // existing strings should not be modified
        assert!(interner.get(apple).unwrap().eq(&String::from("apple")));
        assert!(interner.get(banana).unwrap().eq(&String::from("banana")));
        assert!(interner.get(cherry).unwrap().eq(&String::from("cherry")));

        assert_eq!(None, interner.get(4));
    }
}
