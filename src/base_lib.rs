use atom::Atom;
use parser::tokenize;
use errors::Error;

pub fn library() -> Result<Atom, Error> {
    let lib = include_str!("../lib/lib.myco");
    tokenize(lib)
}
