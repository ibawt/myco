#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Atom(Atom),
    Node(Node),
    Proc(Procedure)
}

use std::fmt;

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Expr::*;
        match self {
            &Atom(ref a) => write!(f, "{}", a),
            &Node(ref n) => write!(f, "{}", n),
            &Proc(ref p) => write!(f, "procedure")
        }
    }
}

pub type ExprResult = Result<Expr, Error>;

use std::collections::HashMap;
