use std::ops::*;

#[derive(Debug, PartialOrd, PartialEq, Clone, Copy)]
pub enum Number {
    Integer(i64),
    Float(f64)
}

use self::Number::*;
use std::fmt;

impl Number {
    pub fn is_zero(self) -> bool {
        match self {
            Integer(i) => i == 0,
            Float(f) => f == 0.0
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Integer(i) => write!(f, "{}", i),
            Float(fl) => write!(f, "{}", fl)
        }
    }
}

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

#[cfg(test)]
mod tests {
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
}
