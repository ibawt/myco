use atom::*;
use parser::*;
use procedure::*;
use errors::*;

// #[derive(Debug, Clone, PartialEq)]
// pub enum Expr {
//     Atom(Atom),
//     Node(Node),
//     Proc(Procedure),
//     Macro(Procedure)
// }

// use std::fmt;

// impl fmt::Display for Expr {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         use self::Expr::*;
//         match *self {
//             Atom(ref a) => write!(f, "{}", a),
//             Node(ref n) => write!(f, "{}", n),
//             Proc(_) => write!(f, "procedure"),
//             Macro(_) => write!(f, "macro")
//         }
//     }
// }

// pub type ExprResult = Result<Expr, Error>;
