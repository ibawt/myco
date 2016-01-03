use errors::Error;
use number::*;
use symbol;
use env::Env;
#[derive (Debug, Clone, PartialEq, Copy)]
pub enum Form {
    Def,
    Do,
    Macro,
    Fn,
    Quote,
    If,
    QuasiQuote,
    Unquote,
    Splice,
    MacroExpand
}

impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Form::*;

        let s = match *self {
            Def => "def",
            Do => "do",
            Macro => "defmacro",
            Fn => "fn",
            Quote => "quote",
            Unquote => "unquote",
            Splice => "splice",
            MacroExpand => "macroexpand",
            If => "if",
            QuasiQuote => "quasiquote"
        };
        write!(f, "{}", s)
    }
}


#[derive (Debug, Clone, PartialEq)]
pub struct Procedure {
    pub params: List,
    pub body: List,
    pub closures: Env
}

#[derive (Debug, Clone, PartialEq)]
pub enum Function {
    Native(Native),
    Proc(Procedure),
    Macro(Procedure)
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Function::*;

        match *self {
            Native(n) => write!(f, "{}", n),
            Proc(_) => write!(f, "proc"),
            Macro(_) => write!(f, "macro")
        }
    }
}

pub type List = Vec<Atom>;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    List(List),
    String(String),
    Number(Number),
    Symbol(symbol::InternedStr),
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
    Rest,
    Not,
    List,
    Cons,
    Append,
    Print
}

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Native::*;

        match *self {
            Add => write!(f, "+"),
            Sub => write!(f, "-"),
            Equal => write!(f, "="),
            GreaterThanOrEqual => write!(f, ">="),
            GreaterThan => write!(f, ">"),
            LessThan => write!(f, "<"),
            LessThanOrEqual => write!(f, "<="),
            Mul => write!(f, "*"),
            Div => write!(f, "/"),
            First => write!(f, "first"),
            Rest => write!(f, "rest"),
            Not => write!(f, "not"),
            List => write!(f, "list"),
            Append => write!(f, "append"),
            Cons => write!(f, "cons"),
            Print => write!(f, "print")
        }
    }
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
    #[allow(dead_code)]
    pub fn string(s: &str) -> Atom {
        Atom::String(s.to_string())
    }

    #[allow(dead_code)]
    pub fn symbol(s: &str) -> Atom {
        Atom::Symbol(symbol::intern(s))
    }

    pub fn is_pair(&self) -> bool {
        match *self {
            Atom::List(ref list) => !list.is_empty(),
            _ => false
        }
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Atom::*;
        match *self {
            List(ref list) => {
                try!(write!(f, "("));
                if let Some(first) = list.first() {
                    try!(write!(f, "{}", first));
                    for a in list.iter().skip(1) {
                        try!(write!(f, " {}", a));
                    }
                }
                write!(f, ")")
            },
            Form(form) => {
                write!(f, "{}", form)
            }
            Function(ref func) => {
                write!(f, "{}", func)
            }
            String(ref s) => {
                write!(f, "\"{}\"", s)
            },
            Nil => {
                write!(f, "nil")
            },
            Number(n) => {
                write!(f, "{}", n)
            },
            Boolean(b) => write!(f, "{}", b),
            Symbol(ref s) => write!(f, "{}", s)
        }
    }
}

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
        "not" => Not,
        "list" => List,
        "append" => Append,
        "cons" => Cons,
        "print" => Print,
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
        "macroexpand" => MacroExpand,
        "defmacro" => Macro,
        _ => return None
    };
    Some(Atom::Form(form))
}

fn find_special_atoms(t: &str) -> Option<Atom> {
    let atom = match t {
        "true" => Atom::Boolean(true),
        "false" => Atom::Boolean(false),
        "nil" =>  Atom::Nil,
        _ => return None
    };
    Some(atom)
}

macro_rules! some {
    ($e:expr) => (match $e { Some(e) => return e, None => ()})
}

fn default_parse(token: &str) -> Atom {
    some!(token.parse::<i64>().ok().map(|n| Atom::Number(Number::Integer(n))));
    token.parse::<f64>().ok()
          .map_or_else(
              || Atom::Symbol(symbol::intern(token)),
              |f| Atom::Number(Number::Float(f)))
}


impl Atom {
    pub fn parse(token: &str) -> Atom {
        some!(find_form(token));
        some!(find_native(token));
        some!(find_special_atoms(token));
        default_parse(token)
    }
}

#[cfg(test)]
mod test {
    use super::Atom;
    use number::Number::*;

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Number(Integer(32)), Atom::parse("32"));
        assert_eq!(Atom::symbol("symbol"), Atom::parse("symbol"));
        assert_eq!(Atom::Boolean(true), Atom::parse("true"));
        assert_eq!(Atom::Boolean(false), Atom::parse("false"));
        assert_eq!(Atom::Nil, Atom::parse("nil"));
    }
}
