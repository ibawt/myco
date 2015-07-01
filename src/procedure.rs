use atom::*;
use parser::*;

#[derive (Debug, Clone, PartialEq)]
pub struct Procedure {
    pub params: Vec<Atom>,
    pub body: Node,
}
