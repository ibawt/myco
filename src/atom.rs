use errors::Error;
use std::ops::*;
use std::collections::*;

#[derive(Debug, PartialOrd, PartialEq, Clone, Copy)]
pub enum Number {
    Integer(i64),
    Float(f64)
}

use self::Number::*;

impl Add for Number {
    type Output = Number;

    fn add(self, other: Number) -> Number {
        match self {
            Integer(i) => {
                match other {
                    Integer(j) => Integer(i + j),
                    Float(j) => Float((i as f64) + j)
                }
            },
            Float(i) => {
                match other {
                    Integer(j) => Float(i + (j as f64)),
                    Float(j) => Float(i + j)
                }
            }
        }
    }
}

impl Sub for Number {
    type Output = Number;

    fn sub(self, other: Number) -> Number {
        match self {
            Integer(i) => {
                match other {
                    Integer(j) => Integer(i - j),
                    Float(j) => Float((i as f64) - j)
                }
            },
            Float(i) => {
                match other {
                    Integer(j) => Float(i - (j as f64)),
                    Float(j) => Float(i - j)
                }
            }
        }
    }
}

impl Mul for Number {
    type Output = Number;

    fn mul(self, other: Number) -> Number {
        match self {
            Integer(i) => {
                match other {
                    Integer(j) => Integer(i * j),
                    Float(j) => Float((i as f64) * j)
                }
            },
            Float(i) => {
                match other {
                    Integer(j) => Float(i * (j as f64)),
                    Float(j) => Float(i * j)
                }
            }
        }
    }
}

impl Div for Number {
    type Output = Number;

    fn div(self, other: Number) -> Number {
        match self {
            Integer(i) => {
                match other {
                    Integer(j) => Integer(i / j),
                    Float(j) => Float((i as f64) / j)
                }
            },
            Float(i) => {
                match other {
                    Integer(j) => Float(i / (j as f64)),
                    Float(j) => Float(i / j)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    List(VecDeque<Atom>),
    String(String),
    Number(Number),
    Symbol(String),
    Boolean(bool),
    Nil
}

pub type AtomResult = Result<Atom, Error>;

impl Atom {
    pub fn parse(token: &str) -> Atom {
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
    use super::Number::*;

    #[test]
    fn adds() {
        assert_eq!(Integer(10), Integer(5) + Integer(5));
        assert_eq!(Integer(15), Integer(5) + Integer(5) + Integer(5));
    }

    #[test]
    fn subs() {
        assert_eq!(Integer(0), Integer(5) - Integer(5));
        assert_eq!(Integer(-10), Integer(0) - Integer(10));
    }

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Number(Integer(32)), Atom::parse("32"));
        assert_eq!(Atom::Symbol("symbol".to_string()), Atom::parse("symbol"));
        assert_eq!(Atom::Boolean(true), Atom::parse("true"));
        assert_eq!(Atom::Boolean(false), Atom::parse("false"));
        assert_eq!(Atom::Nil, Atom::parse("nil"));
    }
}
