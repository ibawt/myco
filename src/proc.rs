#[derive (Debug, Clone, PartialEq)]
pub struct Procedure {
    params: Vec<Atom>,
    body: Node,
}
