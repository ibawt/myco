use atom::*;
use symbol;

#[derive (Debug, Clone, PartialEq)]
struct Entry {
    key: symbol::InternedStr,
    value: Atom
}

#[derive (Debug, Clone)]
pub struct Env {
    def_map: Vec<Vec<Entry>>
}

impl Env {
    pub fn new() -> Env {
        Env { def_map: vec![vec![]] }
    }

    fn find(&self, key: &str) -> Option<&Atom> {
        for env_map in self.def_map.iter().rev() {
            if let Some(entry) = env_map.iter().find(|entry| entry.key.as_ref() == key ) {
                return Some(&entry.value)
            }
        }
        None
    }

    pub fn apply(&mut self, params: &[Atom], args: &[Atom]) {
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

        self.def_map.push(arg_map);
    }

    pub fn pop(&mut self) {
        self.def_map.pop();
    }

    pub fn get(&self, key: &str) -> Option<Atom> {
        self.find(key).map(|x| x.clone())
    }

    pub fn set(&mut self, key: symbol::InternedStr, value: Atom) {
        if let Some(v) = self.def_map.last_mut() {
            v.push( Entry { key: key, value: value } );
        }
    }

    pub fn resolve_symbols(&self, v: &[Atom]) -> Vec<Atom> {
        v.iter().map(|x| self.resolve_atom(x)).collect()
    }

    pub fn resolve_atom(&self, a: &Atom) -> Atom {
        match *a {
            Atom::Symbol(ref s ) => self.get(s).unwrap_or(Atom::Nil),
            _ => a.clone()
        }
    }
}

#[cfg(test)]
mod tests {
    use self::super::*;
    use atom::*;

    #[test]
    fn nested_get() {
        let mut env = Env::new();

        let params = vec![Atom::symbol("a"), Atom::symbol("b")];
        let args = vec![Atom::from(0), Atom::from(1)];

        env.apply(&params, &args);

        assert_eq!(Atom::from(0), env.get("a").unwrap());

        env.apply(&params, &vec![Atom::from(5)]);

        assert_eq!(Atom::from(5), env.get("a").unwrap());

        assert_eq!(Atom::from(1), env.get("b").unwrap());
    }

    #[test]
    fn splat() {
        let mut env = Env::new();

        let params = vec![Atom::symbol("&"), Atom::symbol("body")];

        env.apply(&params, &vec![Atom::from(0), Atom::from(1), Atom::from(2)]);

        let body = env.get("body").unwrap();

        assert_eq!(Atom::List(vec![Atom::from(0), Atom::from(1), Atom::from(2)]), body);
    }

}
