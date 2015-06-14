use std::ops::*;
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
