#[derive(Debug)]
pub enum Error {
    UnexpectedType,
    Parser,
    InvalidArguments,
    NotAFunction,
    NotEnoughArguments,
    NotImplemented
}
