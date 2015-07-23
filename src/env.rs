//////////////////////////////////////////////////////////////////////////////
// Copyright 2015 Ian Quick <ian.quick@gmail.com>                           //
//                                                                          //
// Licensed under the Apache License, Version 2.0 (the "License");          //
// you may not use this file except in compliance with the License.         //
// You may obtain a copy of the License at                                  //
//                                                                          //
//   http://www.apache.org/licenses/LICENSE-2.0                             //
//                                                                          //
// Unless required by applicable law or agreed to in writing, software      //
// distributed under the License is distributed on an "AS IS" BASIS,        //
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. //
// See the License for the specific language governing permissions and      //
// limitations under the License.                                           //
//////////////////////////////////////////////////////////////////////////////
use std::collections::HashMap;

use atom::*;
use errors::*;

#[derive (Debug, Clone)]
pub struct Env {
    def_map: Vec<HashMap<String, Atom>>,
}

impl Env {
    pub fn new() -> Env {
        Env{ def_map: vec![HashMap::new()]}
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
        let mut arg_map = HashMap::new();

        let mut params_iter = params.iter();
        let mut args_iter = args.iter();

        while let Some(&Atom::Symbol(ref sym)) = params_iter.next() {
            if sym == "&" {
                if let Some(&Atom::Symbol(ref splat)) = params_iter.next() {
                    let vals = args_iter.map(|a| a.clone()).collect();
                    arg_map.insert(splat.clone(), Atom::List(vals));
                    break;
                }
            } else {
                args_iter.next().and_then(|arg| arg_map.insert(sym.clone(), arg.clone()));
            }
        }

        self.def_map.push(arg_map);

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

        env.apply(&params, &args).unwrap();

        assert_eq!(Atom::from(0), env.get("a").unwrap());

        env.apply(&params, &vec![Atom::from(5)]).unwrap();

        assert_eq!(Atom::from(5), env.get("a").unwrap());

        assert_eq!(Atom::from(1), env.get("b").unwrap());
    }

    #[test]
    fn splat() {
        let mut env = Env::new();

        let params = vec![Atom::symbol("&"), Atom::symbol("body")];

        env.apply(&params, &vec![Atom::from(0), Atom::from(1), Atom::from(2)]).unwrap();

        let body = env.get("body").unwrap();

        assert_eq!(Atom::List(vec![Atom::from(0), Atom::from(1), Atom::from(2)]), body);
    }

}
