use self::Error::*;
use std::io;
use std::error;
use std::fmt;

#[derive(Debug)]
#[allow(dead_code)]
pub enum Error {
    EoF,
    UnexpectedType,
    Parser,
    InvalidArguments(String),
    NotAFunction,
    RuntimeAssertion,
    NotEnoughArguments,
    NotImplemented,
    Io(io::Error)
}

use std::cmp::PartialEq;

pub fn invalid_arg(s: &str) -> Error {
    Error::InvalidArguments(s.to_string())
}

impl PartialEq for Error {
    fn eq(&self, r: &Error) -> bool {
        match (self, r) {
            (&EoF, &EoF) => true,
            _ => false
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Error {
        Error::Io(e)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            RuntimeAssertion => "RuntimeAssertion",
            EoF => "End of File",
            UnexpectedType => "UnexpectedType",
            Parser => "Parser",
            InvalidArguments(ref s) => s,
            NotAFunction => "NotAFunction",
            NotEnoughArguments => "NotEnoughArguments",
            NotImplemented => "NotImplemented",
            Io(ref e) => e.description()
        }
    }
}
