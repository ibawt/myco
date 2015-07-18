use std::collections::HashMap;

use atom::*;
use errors::*;
use errors::Error::*;

#[derive (Debug,Clone)]
pub struct Env {
    def_map: Vec<HashMap<String, Atom>>,
}

impl Env {
    pub fn new() -> Env {
        Env{ def_map: vec![HashMap::new()]}
    }

    pub fn depth(&self) -> usize {
        self.def_map.len()
    }

    fn find(&self, key: &str, i: usize) -> Option<&Atom> {
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

    pub fn apply(&mut self, params: &[Atom], args: &[Atom]) -> Result<(),Error> {
        if params.len() != args.len() {
            return Err(InvalidArguments);
        }

        let mut m = HashMap::new();

        // let mut p_iter = params.iter();
        // let mut a_iter = args.iter();

        // loop {
        //     match p_iter.next() {
        //         Some(&Atom::Symbol(ref sym)) => {
        //             if sym == "&" {
        //                 if let Some(&Atom::Symbol(ref splat)) = p_iter.next() {
        //                     let mut vals = vec![];
        //                     while let Some(a) = a_iter.next() {
        //                         vals.push(a.clone());
        //                     }
        //                     m.insert(splat.clone(), Atom::List(vals));
        //                 }
        //             } else {
        //                 if let Some(arg) = a_iter.next() {
        //                     m.insert(sym.clone(), arg.clone())
        //                 }
        //             }
        //             ()
        //         },
        //         None => {
        //             break;
        //         }
        //     }
        // }
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

    pub fn pop(&mut self) {
        self.def_map.pop();
    }

    pub fn get(&self, key: &str) -> Option<Atom> {
        self.find(key, self.def_map.len() - 1 ).map(|x| x.clone())
    }

    pub fn set(&mut self, key: String, value: Atom) {
        if let Some(map) = self.def_map.last_mut() {
            map.insert(key, value);
        }
    }

    pub fn resolve_symbols(&self, v: &[Atom]) -> Vec<Atom> {
        v.iter()
            .map(|x| match *x {
                Atom::Symbol(ref s) => {
                    match self.get(s) {
                        Some(d) => d.clone(),
                        _ => Atom::Nil
                    }
                },
                _ => x.clone()
            }).collect()
    }
}

