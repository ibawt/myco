use atom::*;
use env::*;
use eval::*;
use parser::*;
use errors::Error;

pub fn init(env: &mut Env) -> Result<Atom, Error> {
    let lib = include_str!("../lib/lib.lisp");

    tokenize(lib)
        .and_then(|node| eval(node, env))
}
