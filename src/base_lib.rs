use atom::*;
use env::*;
use parser::*;
use errors::Error;
use vm;

pub fn init(env: &mut Env) -> Result<Atom, Error> {
    let lib = include_str!("../lib/lib.lisp");

    tokenize(lib).and_then(|node| vm::default_run_node(node, env))
}
