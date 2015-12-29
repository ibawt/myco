#[derive(Debug, PartialEq)]
pub enum Error {
    EoF,
    UnexpectedType,
    Parser,
    InvalidArguments,
    NotAFunction,
    NotEnoughArguments,
    NotImplemented
}
use std::error;
use std::fmt;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

use self::Error::*;

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            EoF => "End of File",
            UnexpectedType => "UnexpectedType",
            Parser => "Parser",
            InvalidArguments => "InvalidArguments",
            NotAFunction => "NotAFunction",
            NotEnoughArguments => "NotEnoughArguments",
            NotImplemented => "NotImplemented"
        }
    }
}
