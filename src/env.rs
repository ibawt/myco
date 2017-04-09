use atom::*;
use symbol;
use errors::*;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

#[derive (Debug, Clone, PartialEq)]
struct Entry {
    key: symbol::InternedStr,
    value: Atom,
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.key, self.value)
    }
}

#[derive(Debug,PartialEq)]
struct EnvGeneration {
    value: Vec<Entry>,
    parent: Option<Env>,
}

impl fmt::Display for EnvGeneration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(writeln!(f, "EnvGeneration[{:p}]", self));
        for i in &self.value {
            try!(write!(f, "\t{}", i));
        }
        Ok(())
    }
}

impl EnvGeneration {
    fn new(parent: Option<Env>) -> EnvGeneration {
        EnvGeneration {
            value: Vec::new(),
            parent: parent,
        }
    }

    fn find(&self, key: &str) -> Option<&Entry> {
        self.value.iter().rev().find(|entry| entry.key.as_ref() == key)
    }
}


#[derive (Debug, Clone, PartialEq)]
pub struct Env(Rc<RefCell<EnvGeneration>>);

impl Default for Env {
    fn default() -> Env {
        Env::new(None)
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let gen = self.0.borrow();
        try!(write!(f, "Top-level: EnvGeneration: {}\n", *gen));

        if let Some(ref parent) = gen.parent {
            try!(write!(f, "parent: {}\n", parent));
        } else {
            try!(write!(f, "no parent\n"));
        }
        Ok(())
    }
}

impl Env {
    pub fn new(parent: Option<Env>) -> Env {
        Env(Rc::new(RefCell::new(EnvGeneration::new(parent))))
    }

    fn find(&self, key: &str) -> Option<Atom> {
        let gen = self.0.borrow();
        let value = &gen.value;
        match value.iter().find(|entry| entry.key.as_ref() == key) {
            Some(ref entry) => Some(entry.value.clone()),
            None => {
                if let Some(ref parent) = gen.parent {
                    parent.find(key)
                } else {
                    None
                }
            }
        }
    }

    #[allow(dead_code)]
    pub fn num_generations(&self) -> usize {
        self.num_gen(1)
    }

    fn num_gen(&self, c: usize) -> usize {
        let gen = self.0.borrow();
        if let Some(ref parent) = gen.parent {
            parent.num_gen(c + 1)
        } else {
            c
        }
    }

    pub fn bind_mut(&mut self, params: &[Atom], args: &[Atom]) {
        let mut params_iter = params.iter();
        let mut args_iter = args.iter();
        while let Some(&Atom::Symbol(ref sym)) = params_iter.next() {
            if sym.as_ref() == "&" {
                if let Some(&Atom::Symbol(ref splat)) = params_iter.next() {
                    let vals = args_iter.cloned().collect();
                    self.0.borrow_mut().value.push(Entry {
                        key: *splat,
                        value: Atom::list(vals),
                    });
                    break;
                }
            } else {
                let arg = args_iter.next().cloned().unwrap_or(Atom::Nil);
                {
                    // tricking the borrow checker here
                    let mut gen = self.0.borrow_mut();

                    if let Some(mut entry) = gen.value.iter_mut().find(|entry| entry.key == *sym) {
                        entry.value = arg;
                        continue;
                    }
                }
                self.0.borrow_mut().value.push(Entry {
                    key: *sym,
                    value: arg,
                });
            }
        }
    }

    pub fn bind(&self, params: &[Atom], args: &[Atom]) -> Env {
        let mut e = Env::new(Some(self.clone()));
        e.bind_mut(params, args);
        e
    }

    pub fn get(&self, key: &str) -> Option<Atom> {
        self.find(key)
    }

    pub fn define(&mut self, key: symbol::InternedStr, value: Atom) -> Result<Atom> {
        if let Some(_) = self.0.borrow().find(key.as_ref()) {
            bail!(format!("define has entry already: {}", key.as_ref()));
        }
        self.0.borrow_mut().value.push(Entry {
            key: key,
            value: value,
        });
        Ok(Atom::Symbol(key))
    }

    pub fn set(&mut self, key: symbol::InternedStr, value: Atom) -> Result<Atom> {
        {
            // set in current generation
            let mut gen = self.0.borrow_mut();
            if let Some(mut entry) = gen.value.iter_mut().find(|entry| entry.key == key) {
                entry.value = value;
                return Ok(Atom::Symbol(key));
            }
        }

        if let Some(ref mut parent) = self.0.borrow_mut().parent {
            parent.set(key, value)
        } else {
            bail!("in set")
        }
    }
}

#[cfg(test)]
mod tests {
    use self::super::*;
    use symbol::*;

    #[test]
    fn nested_get() {
        let mut env = Env::new(None);

        env.define(intern("foo"), Atom::String("bar".to_owned())).unwrap();

        let env2 = Env::new(Some(env.clone()));

        assert_eq!(Atom::String("bar".to_owned()),
                   env2.get(intern("foo").as_ref()).unwrap());
    }

    #[test]
    fn splat() {
        let env = Env::new(None);

        let params = vec![Atom::symbol("&"), Atom::symbol("body")];

        let e = env.bind(&params, &vec![Atom::from(0), Atom::from(1), Atom::from(2)]);

        let body = e.get("body").unwrap();

        assert_eq!(Atom::list(vec![Atom::from(0), Atom::from(1), Atom::from(2)]),
                   body);
    }

    #[test]
    fn define() {
        let mut env = Env::new(None);

        env.define(intern("foo"), Atom::string("bar")).unwrap();

        // if let Error(ErrorKind::InvalidArguments(_)) = env.set(intern("bar"), Atom::string("foo")) {
        //     assert!(true);
        // } else {
        //     assert!(false);
        // }

        let mut env2 = Env::new(Some(env.clone()));

        env2.set(intern("foo"), Atom::parse("0")).unwrap();

        assert_eq!(Atom::parse("0"), env.get("foo").unwrap());
        assert_eq!(Atom::parse("0"), env2.get("foo").unwrap());

        env2.define(intern("foo"), Atom::string("baz")).unwrap();

        assert_eq!(Atom::string("baz"), env2.get("foo").unwrap());
        assert_eq!(Atom::parse("0"), env.get("foo").unwrap());
    }

    #[test]
    fn bind_mut() {
        let mut env = Env::default();
        let args = vec![Atom::from(0), Atom::from(1)];
        let params = vec![Atom::symbol("a"), Atom::symbol("b")];
        env.bind_mut(&params, &args);

        assert_eq!(env.get("a").unwrap(), Atom::from(0));
        assert_eq!(env.get("b").unwrap(), Atom::from(1));

        env.bind_mut(&params, &vec![Atom::from(2), Atom::from(42)]);


        assert_eq!(env.get("a").unwrap(), Atom::from(2));
        assert_eq!(env.get("b").unwrap(), Atom::from(42));

        assert_eq!(2, env.0.borrow().value.len());
    }

}
