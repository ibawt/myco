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
    InvalidArguments,
    NotAFunction,
    RuntimeAssertion,
    NotEnoughArguments,
    NotImplemented,
    Io(io::Error)
}

use std::cmp::PartialEq;

impl PartialEq for Error {
    fn eq(&self, r: &Error) -> bool {
        match (self, r) {
            (&EoF, &EoF) => true,
            (&UnexpectedType, &UnexpectedType) => true,
            (&Parser, &Parser) => true,
            (&InvalidArguments, &InvalidArguments) => true,
            (&NotAFunction, &NotAFunction) => true,
            (&RuntimeAssertion, &RuntimeAssertion) => true,
            (&NotEnoughArguments, &NotEnoughArguments) => true,
            (&NotImplemented, &NotImplemented) => true,
            (&Io(_), &Io(_)) => true,
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
            InvalidArguments => "InvalidArguments",
            NotAFunction => "NotAFunction",
            NotEnoughArguments => "NotEnoughArguments",
            NotImplemented => "NotImplemented",
            Io(ref e) => e.description()
        }
    }
}
