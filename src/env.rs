use atom::*;
use symbol;

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug,PartialEq)]
struct EnvGeneration {
    value: Vec<Entry>,
    parent: Option<Env>
}

impl EnvGeneration {
    fn new(parent: Option<Env>) -> EnvGeneration {
        EnvGeneration {
            value: Vec::new(),
            parent: parent
        }
    }
}

#[derive (Debug, Clone, PartialEq)]
struct Entry {
    key: symbol::InternedStr,
    value: Atom
}

#[derive (Debug, Clone, PartialEq)]
pub struct Env(Rc<RefCell<EnvGeneration>>);

impl Env {
    pub fn new(parent: Option<Env>) -> Env {
        Env(Rc::new(RefCell::new(EnvGeneration::new(parent))))
    }

    fn find(&self, key: &str) -> Option<Atom> {
        let gen = self.0.borrow();
        let value = &gen.value;
        match value.iter().find(|entry| entry.key.as_ref() == key) {
            Some(ref entry) => {
                Some(entry.value.clone())
            },
            None => {
                if let Some(ref parent) = gen.parent {
                    parent.find(key)
                } else {
                    None
                }
            }
        }
    }

    pub fn bind(&mut self, params: &[Atom], args: &[Atom]) -> Env {
        let mut arg_map = vec![];
        let mut params_iter = params.iter();
        let mut args_iter = args.iter();

        while let Some(&Atom::Symbol(ref sym)) = params_iter.next() {
            if sym.as_ref() == "&" {
                if let Some(&Atom::Symbol(ref splat)) = params_iter.next() {
                    let vals = args_iter.map(|a| a.clone()).collect();
                    arg_map.push(Entry { key: splat.clone(), value: Atom::List(vals) });
                    break;
                }
            } else {
                args_iter.next().map(|arg| {
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

    pub fn set(&mut self, key: symbol::InternedStr, value: Atom) {
        self.0.borrow_mut().value.push(Entry {
            key: key,
            value: value
        });
    }
}

#[cfg(test)]
mod tests {
    use self::super::*;
    use atom::*;
    use symbol::*;

    #[test]
    fn nested_get() {
        let mut env = Env::new(None);

        env.set(intern("foo"), Atom::String("bar".to_owned()));

        let env2 = Env::new(Some(env.clone()));

        assert_eq!(Atom::String("bar".to_owned()), env2.get(intern("foo").as_ref()).unwrap());
    }

    #[test]
    fn splat() {
        let mut env = Env::new(None);

        let params = vec![Atom::symbol("&"), Atom::symbol("body")];

        let e = env.bind(&params, &vec![Atom::from(0), Atom::from(1), Atom::from(2)]);

        let body = e.get("body").unwrap();

        assert_eq!(Atom::List(vec![Atom::from(0), Atom::from(1), Atom::from(2)]), body);
    }

}
