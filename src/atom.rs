use errors::Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Atom {
    Integer(i64),
    Float(f64),
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
            Ok(v) => Atom::Integer(v),
            _ => {
                let f = token.parse::<f64>();
                match f {
                    Ok(x) => Atom::Float(x),
                    _ => Atom::Symbol(token.to_string())
                }
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::Atom;

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Integer(32), Atom::parse("32"));
        assert_eq!(Atom::Symbol("symbol".to_string()), Atom::parse("symbol"));
        assert_eq!(Atom::Boolean(true), Atom::parse("true"));
        assert_eq!(Atom::Boolean(false), Atom::parse("false"));
        assert_eq!(Atom::Nil, Atom::parse("nil"));
    }
}
