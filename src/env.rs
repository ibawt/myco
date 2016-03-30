use atom::*;
use symbol;
use errors::*;
use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug,PartialEq)]
struct EnvGeneration {
    value: Vec<Entry>,
    parent: Option<Env>
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
            parent: parent
        }
    }

    fn find(&self, key: &str) -> Option<&Entry> {
        self.value.iter().find(|entry| entry.key.as_ref() == key )
    }
}

#[derive (Debug, Clone, PartialEq)]
struct Entry {
    key: symbol::InternedStr,
    value: Atom
}

impl fmt::Display for Entry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} => {}", self.key, self.value)
    }
}

#[derive (Debug, Clone, PartialEq)]
pub struct Env(Rc<RefCell<EnvGeneration>>);

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
            Some(ref entry) => {
                // println!("found {} it here in: {:p}", key, self);
                Some(entry.value.clone())
            },
            None => {
                if let Some(ref parent) = gen.parent {
                    // println!("looking for {} in parent: {}", key, parent);
                    parent.find(key)
                } else {
                    None
                }
            }
        }
    }

    pub fn bind_mut(&mut self, params: &[Atom], args: &[Atom]) {
        let mut params_iter = params.iter();
        let mut args_iter = args.iter();

        while let Some(&Atom::Symbol(ref sym)) = params_iter.next() {
            if sym.as_ref() == "&" {
                if let Some(&Atom::Symbol(ref splat)) = params_iter.next() {
                    let vals = args_iter.map(|a| a.clone()).collect();
                    self.0.borrow_mut().value.push(Entry {
                        key: splat.clone(),
                        value: Atom::list(vals)
                    });
                    // arg_map.push(Entry { key: splat.clone(), value: Atom::list(vals) });
                    break;
                }
            } else {
                args_iter.next().map(|arg| {
                    // println!("setting {} to {} in {}", sym, arg, self);
                    self.0.borrow_mut().value.push(Entry {
                        key: *sym,
                        value: arg.clone()
                    });
                    // self.define()
                    // arg_map.push(Entry { key: sym.clone(), value: arg.clone() });
                });
            }
        }

        // let env_gen = EnvGeneration {
        //     value: arg_map,
        //     parent: Some(self.clone())
        // };
        // Env(Rc::new(RefCell::new(env_gen)))
    }

    pub fn bind(&mut self, params: &[Atom], args: &[Atom]) -> Env {
        let mut arg_map = Vec::with_capacity(params.len());
        let mut params_iter = params.iter();
        let mut args_iter = args.iter();

        while let Some(&Atom::Symbol(ref sym)) = params_iter.next() {
            if sym.as_ref() == "&" {
                if let Some(&Atom::Symbol(ref splat)) = params_iter.next() {
                    let vals = args_iter.map(|a| a.clone()).collect();
                    arg_map.push(Entry { key: splat.clone(), value: Atom::list(vals) });
                    break;
                }
            } else {
                args_iter.next().map(|arg| {
                    // println!("setting {} to {}", sym, arg);
                    arg_map.push(Entry { key: sym.clone(), value: arg.clone() });
                });
            }
        }

        let env_gen = EnvGeneration {
            value: arg_map,
            parent: Some(self.clone())
        };
        Env(Rc::new(RefCell::new(env_gen)))
    }

    pub fn get(&self, key: &str) -> Option<Atom> {
        self.find(key)
    }

    pub fn define(&mut self, key: symbol::InternedStr, value: Atom) -> Result<Atom, Error> {
        if let Some(_) = self.0.borrow().find(key.as_ref()) {
            // println!("env is: {}", self);
            return Err(invalid_arg(&format!("define has entry already: {}", key.as_ref())))
        }
        self.0.borrow_mut().value.push(Entry {
            key: key,
            value: value
        });
        Ok(Atom::Symbol(key))
    }

    pub fn set(&mut self, key: symbol::InternedStr, value: Atom) -> Result<Atom, Error> {
        {
            // set in current generation
            let mut gen = self.0.borrow_mut();
            match gen.value.iter_mut().find(|entry| entry.key == key) {
                Some(mut entry) => {
                    entry.value = value;
                    return Ok(Atom::Symbol(key))
                },
                _ => ()
            }
        }

        if let Some(ref mut parent) = self.0.borrow_mut().parent {
            parent.set(key, value)
        } else {
            Err(invalid_arg("in set"))
        }
    }
}

#[cfg(test)]
mod tests {
    use self::super::*;
    use atom::*;
    use errors::*;
    use symbol::*;

    #[test]
    fn nested_get() {
        let mut env = Env::new(None);

        env.define(intern("foo"), Atom::String("bar".to_owned())).unwrap();

        let env2 = Env::new(Some(env.clone()));

        assert_eq!(Atom::String("bar".to_owned()), env2.get(intern("foo").as_ref()).unwrap());
    }

    #[test]
    fn splat() {
        let mut env = Env::new(None);

        let params = vec![Atom::symbol("&"), Atom::symbol("body")];

        let e = env.bind(&params, &vec![Atom::from(0), Atom::from(1), Atom::from(2)]);

        let body = e.get("body").unwrap();

        assert_eq!(Atom::list(vec![Atom::from(0), Atom::from(1), Atom::from(2)]), body);
    }

    #[test]
    fn define() {
        let mut env = Env::new(None);

        env.define(intern("foo"), Atom::string("bar")).unwrap();

        assert_eq!(Err(Error::InvalidArguments), env.set(intern("bar"), Atom::string("foo")));

        let mut env2 = Env::new(Some(env.clone()));

        env2.set(intern("foo"), Atom::parse("0")).unwrap();

        assert_eq!(Atom::parse("0"), env.get("foo").unwrap());
        assert_eq!(Atom::parse("0"), env2.get("foo").unwrap());

        env2.define(intern("foo"), Atom::string("baz")).unwrap();

        assert_eq!(Atom::string("baz"), env2.get("foo").unwrap());
        assert_eq!(Atom::parse("0"), env.get("foo").unwrap());
    }

}
