use errors::Error;
use number::*;
use symbol;
use env::Env;
use opcodes::Opcode;

#[derive (Debug, Clone, PartialEq, Copy)]
pub enum Form {
    Def,
    Let,
    Set,
    Do,
    Macro,
    Fn,
    Quote,
    If,
    QuasiQuote,
    Unquote,
    Splice,
    MacroExpand,
    Eval,
    Recur,
}

impl fmt::Display for Form {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Form::*;

        let s = match *self {
            Let => "let*",
            Def => "def",
            Set => "set!",
            Do => "do",
            Macro => "defmacro",
            Fn => "fn",
            Quote => "quote",
            Unquote => "unquote",
            Splice => "splice",
            MacroExpand => "macroexpand",
            If => "if",
            QuasiQuote => "quasiquote",
            Eval => "eval",
            Recur => "recur",
        };
        write!(f, "{}", s)
    }
}


#[derive (Debug, Clone, PartialEq)]
pub struct Procedure {
    pub id: u32,
    pub params: List,
    pub body: List,
    pub closures: Env,
}


#[derive (Debug, Clone, PartialEq)]
pub struct CompiledFunction {
    pub body: Vec<Opcode>,
    pub source: List,
    pub params: List,
    pub env: Env,
}

#[derive (Debug, Clone, PartialEq)]
pub enum Function {
    Native(Native),
    Proc(Procedure),
    Compiled(CompiledFunction),
    Macro(Procedure),
}


impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Function::*;
        use opcodes;
        use eval;

        match *self {
            Native(n) => write!(f, "{}", n),
            Proc(_) => write!(f, "proc"),
            Compiled(ref func) => {
                try!(write!(f, "compiled-proc:\n"));
                try!(write!(f, "params: {}\n", eval::print_list(&func.params)));
                try!(write!(f, "source: {}\n", eval::print_list(&func.source)));
                write!(f,
                       "instructions: {}\n",
                       opcodes::print_instructions(&func.body))
            }
            Macro(_) => write!(f, "macro"),
        }
    }
}
use std::rc::Rc;

pub type List = Rc<Vec<Atom>>;

pub fn empty_list() -> List {
    Rc::new(Vec::new())
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    List(List),
    Keyword(symbol::InternedStr),
    String(String),
    Number(Number),
    Symbol(symbol::InternedStr),
    Boolean(bool),
    Function(Function),
    Form(Form),
    Nil,
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
    Print,
    Error,
    Type,
    Slurp,
    Barf,
    Count,
    Apply,
    Get,
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
            Print => write!(f, "print"),
            Error => write!(f, "error"),
            Type => write!(f, "type-of"),
            Slurp => write!(f, "slurp"),
            Barf => write!(f, "barf"),
            Count => write!(f, "count"),
            Apply => write!(f, "apply"),
            Get => write!(f, "get"),
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
            }
            Keyword(s) => write!(f, ":{}", s),
            Form(form) => write!(f, "{}", form),
            Function(ref func) => write!(f, "{}", func),
            String(ref s) => write!(f, "\"{}\"", s),
            Nil => write!(f, "nil"),
            Number(n) => write!(f, "{}", n),
            Boolean(b) => write!(f, "{}", b),
            Symbol(ref s) => write!(f, "{}", s),
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
        "error" => Error,
        "type-of" => Type,
        "slurp" => Slurp,
        "barf" => Barf,
        "count" => Count,
        "apply" => Apply,
        "get" => Get,
        _ => return None,
    };
    Some(Atom::Function(Function::Native(native)))
}

fn find_form(t: &str) -> Option<Atom> {
    use self::Form::*;
    let form = match t {
        "eval" => Eval,
        "let*" => Let,
        "def" => Def,
        "set!" => Set,
        "do" => Do,
        "fn" => Fn,
        "quote" => Quote,
        "if" => If,
        "macroexpand" => MacroExpand,
        "defmacro" => Macro,
        "recur" => Recur,
        _ => return None,
    };
    Some(Atom::Form(form))
}

fn find_special_atoms(t: &str) -> Option<Atom> {
    if t.starts_with(':') {
        return Some(Atom::Keyword(symbol::intern(&t[1..])));
    }

    let atom = match t {
        "true" => Atom::Boolean(true),
        "false" => Atom::Boolean(false),
        "nil" => Atom::Nil,
        _ => return None,
    };
    Some(atom)
}

macro_rules! some {
    ($e:expr) => (if let Some(e) = $e { return e })
}

fn default_parse(token: &str) -> Atom {
    some!(token.parse::<i64>().ok().map(|n| Atom::Number(Number::Integer(n))));
    token.parse::<f64>()
        .ok()
        .map_or_else(|| Atom::Symbol(symbol::intern(token)),
                     |f| Atom::Number(Number::Float(f)))
}


impl Atom {
    pub fn parse(token: &str) -> Atom {
        some!(find_form(token));
        some!(find_native(token));
        some!(find_special_atoms(token));
        default_parse(token)
    }

    pub fn as_bool(&self) -> bool {
        match *self {
            Atom::Boolean(b) => b,
            Atom::Nil => false,
            _ => true,
        }
    }

    pub fn as_string(&self) -> Result<&str, Error> {
        match *self {
            Atom::String(ref s) => Ok(s),
            _ => Err(Error::UnexpectedType),
        }
    }

    pub fn as_symbol(&self) -> Result<&symbol::InternedStr, Error> {
        match *self {
            Atom::Symbol(ref sym) => Ok(sym),
            _ => Err(Error::UnexpectedType),
        }
    }

    pub fn as_list(&self) -> Result<&List, Error> {
        match *self {
            Atom::List(ref l) => Ok(l),
            _ => Err(Error::UnexpectedType),
        }
    }

    pub fn as_number(&self) -> Result<Number, Error> {
        match *self {
            Atom::Number(n) => Ok(n),
            _ => Err(Error::UnexpectedType),
        }
    }

    #[allow(dead_code)]
    pub fn as_function(&self) -> Result<&Function, Error> {
        match *self {
            Atom::Function(ref f) => Ok(f),
            _ => Err(Error::UnexpectedType),
        }
    }

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
            _ => false,
        }
    }

    pub fn list(v: Vec<Atom>) -> Atom {
        Atom::List(Rc::new(v))
    }
}

pub fn to_list(v: Vec<Atom>) -> List {
    Rc::new(v)
}

#[cfg(test)]
mod test {
    use super::Atom;
    use number::Number::*;
    use symbol::intern;

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Number(Integer(32)), Atom::parse("32"));
        assert_eq!(Atom::symbol("symbol"), Atom::parse("symbol"));
        assert_eq!(Atom::symbol("foo-bar"), Atom::parse("foo-bar"));
        assert_eq!(Atom::Boolean(true), Atom::parse("true"));
        assert_eq!(Atom::Boolean(false), Atom::parse("false"));
        assert_eq!(Atom::Nil, Atom::parse("nil"));
        assert_eq!(Atom::Keyword(intern("foo")), Atom::parse(":foo"));
    }
}
