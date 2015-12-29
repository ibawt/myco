use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;

#[derive (Eq, PartialEq, Clone, Copy, Default, Hash, Debug)]
pub struct InternedStr(usize);

#[derive (Debug)]
pub struct Interner {
    indices: HashMap<String, usize>,
    strings: Vec<String>
}

impl Interner {
    pub fn new() -> Interner {
        Interner { indices: HashMap::new(), strings: vec![] }
    }

    pub fn intern(&mut self, s: &str) -> InternedStr {
        match self.indices.get(s).map(|x| *x) {
            Some(index) => InternedStr(index),
            None => {
                let index = self.strings.len();
                self.indices.insert(s.to_string(), index);
                self.strings.push(s.to_string());
                InternedStr(index)
            }
        }
    }

    pub fn get_str(&self, InternedStr(i): InternedStr) -> &str {
        if i < self.strings.len() {
            &*self.strings[i]
        } else {
            panic!("argh!");
        }
    }
}

pub fn get_local_interner() -> Rc<RefCell<Interner>> {
    thread_local!( static INTERNER: Rc<RefCell<Interner>> = Rc::new(RefCell::new(Interner::new())));
    INTERNER.with(|interner| interner.clone())
}

pub fn intern(s: &str) -> InternedStr {
    let interner = get_local_interner();
    let mut i = interner.borrow_mut();
    i.intern(s)
}

impl Deref for InternedStr {
    type Target = str;
    fn deref(&self) -> &str {
        self.as_ref()
    }
}

impl AsRef<str> for InternedStr {
    fn as_ref(&self) -> &str {
        let interner = get_local_interner();
        let x = (*interner).borrow_mut();
        let r: &str = x.get_str(*self);
        unsafe { ::std::mem::transmute(r) }
    }
}
