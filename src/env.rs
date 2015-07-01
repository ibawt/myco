#[derive (Debug,Clone)]
pub struct Env {
    def_map: Vec<HashMap<String, Expr>>,
}

impl Env {
    fn new() -> Env {
        Env{ def_map: vec![HashMap::new()]}
    }

    fn find(&self, key: &str, i: usize) -> Option<&Expr> {
        let map = &self.def_map[i];

        let val = map.get(key);

        match val {
            Some(_) => val,
            None => {
                if i > 0 {
                    self.find(key, i - 1 )
                } else {
                    None
                }
            }
        }
    }

    fn apply(&mut self, params: &[Atom], args: &[Expr]) -> Result<(),Error> {
        if params.len() != args.len() {
            return Err(InvalidArguments);
        }

        let mut m = HashMap::new();

        for p in params.iter().zip(args.iter()) {
            if let &Atom::Symbol(ref sym) = p.0 {
                m.insert(sym.clone(), p.1.clone());
            } else {
                return Err(InvalidArguments);
            }
        }

        self.def_map.push(m);

        Ok(())
    }

    fn pop(&mut self) {
        self.def_map.pop();
    }

    fn get(&self, key: &str) -> Option<&Expr> {
        self.find(key, self.def_map.len() - 1 )
    }

    fn set(&mut self, key: String, value: Expr) {
        if let Some(map) = self.def_map.last_mut() {
            map.insert(key, value);
        }
    }
}

fn resolve_symbols(v: &[Expr], env: &Env) -> Vec<Expr> {
    v.iter()
        .map(|x| match x {
            &Expr::Atom(Atom::Symbol(ref s)) => {
                match env.get(s) {
                    Some(d) => d.clone(),
                    _ => Expr::Atom(Atom::Nil)
                }
            },
            _ => x.clone()
        }).collect::<Vec<Expr>>()
}
