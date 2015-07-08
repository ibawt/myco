use errors::Error;
use number::*;

#[derive (Debug, Clone, PartialEq, Copy)]
pub enum Form {
    Def,
    Do,
    Macro,
    Fn,
    Quote,
    If
}

#[derive (Debug, Clone, PartialEq)]
pub struct Procedure {
    pub params: List,
    pub body: List,
}

#[derive (Debug, Clone, PartialEq)]
pub enum Function {
    Native(Native),
    Proc(Procedure)
}
pub type List = Vec<Atom>;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    List(List),
    String(String),
    Number(Number),
    Symbol(String),
    Boolean(bool),
    Function(Function),
    Form(Form),
    Nil
}

#[derive (Debug, PartialEq, Clone, Copy)]
pub enum Native {
    Add,
    Sub,
    Equal,
    GreaterThanOrEqual,
    GreaterThan,
    LessThan,
    LessThanOrEqual,
    Mul,
    Div,
    First,
    Rest
}

use std::fmt;
use std::convert::From;

impl From<bool> for Atom {
    fn from(b: bool) -> Atom {
        Atom::Boolean(b)
    }
}

impl From<i64> for Atom {
    fn from(i: i64) -> Atom {
        Atom::Number(Number::Integer(i))
    }
}

impl From<f64> for Atom {
    fn from(f: f64) -> Atom {
        Atom::Number(Number::Float(f))
    }
}

impl Atom {
    pub fn string(s: &str) -> Atom {
        Atom::String(s.to_string())
    }

    pub fn symbol(s: &str) -> Atom {
        Atom::Symbol(s.to_string())
    }
}

// impl fmt::Display for Atom {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         use self::Atom::*;
//         match *self {
//             List(ref list) => {
//                 try!(write!(f, "("));
//                 if let Some(first) = list.front() {
//                     try!(write!(f, "{}", first));
//                     for a in list.iter().skip(1) {
//                         try!(write!(f, " {}", a));
//                     }
//                 }
//                 write!(f, ")")
//             },
//             String(ref s) => {
//                 write!(f, "\"{}\"", s)
//             },
//             Nil => {
//                 write!(f, "nil")
//             },
//             Number(n) => {
//                 write!(f, "{}", n)
//             },
//             Boolean(b) => write!(f, "{}", b),
//             Symbol(ref s) => write!(f, "{}", s)
//         }
//     }
// }
//
pub type AtomResult = Result<Atom, Error>;

fn find_native(t: &str) -> Option<Atom> {
    use self::Native::*;
    let native = match t {
        "+" => Add,
        "-" => Sub,
        "=" => Equal,
        ">" => GreaterThan,
        ">=" => GreaterThanOrEqual,
        "<=" => LessThanOrEqual,
        "<" => LessThan,
        "*" => Mul,
        "/" => Div,
        "first" => First,
        "rest" => Rest,
        _ => return None
    };
    Some(Atom::Function(Function::Native(native)))
}

fn find_form(t: &str) -> Option<Atom> {
    use self::Form::*;
    let form = match t {
        "def" => Def,
        "do" => Do,
        "fn" => Fn,
        "quote" => Quote,
        "if" => If,
        _ => return None
    };
    Some(Atom::Form(form))
}

impl Atom {
    pub fn parse(token: &str) -> Atom {
        if let Some(form) = find_form(token) {
            return form
        }

        if let Some(native) = find_native(token) {
            return native
        }
        match token {
            "true" => return Atom::Boolean(true),
            "false" => return Atom::Boolean(false),
            "nil" => return Atom::Nil,
            _ => ()
        }

        let i = token.parse::<i64>();
        match i {
            Ok(v) => Atom::Number(Number::Integer(v)),
            _ => {
                let f = token.parse::<f64>();
                match f {
                    Ok(x) => Atom::Number(Number::Float(x)),
                    _ => Atom::Symbol(token.to_string())
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::Atom;
    use number::Number::*;

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Number(Integer(32)), Atom::parse("32"));
        assert_eq!(Atom::Symbol("symbol".to_string()), Atom::parse("symbol"));
        assert_eq!(Atom::Boolean(true), Atom::parse("true"));
        assert_eq!(Atom::Boolean(false), Atom::parse("false"));
        assert_eq!(Atom::Nil, Atom::parse("nil"));
    }
}
