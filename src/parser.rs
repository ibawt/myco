use atom::*;
use errors::*;
use number::*;
use std::iter::*;
use std::io::prelude::*;

pub fn tokenize(line: &str) -> ParseResult {
    let mut chars = line.chars().peekable();
    return read_tokens(&mut chars);
}

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Atom(Atom),
    List(Vec<Node>),
    Quote,
    SyntaxQuote,
    Splice
}

pub type ParseResult = Result<Node, Error>;

use std::str::Chars;

#[derive (Debug, PartialEq)]
enum Token {
    Atom(Atom),
    Open,        // (
    Close,       // )
    Quote,       // '
    SyntaxQuote, // `
    Unquote,     // ~
    Splice,      // ~@
}

use std::convert::From;

impl Node {
    fn symbol(s: &str) -> Node {
        Node::Atom(Atom::Symbol(s.to_string()))
    }

    fn string(s: &str) -> Node {
        Node::Atom(Atom::String(s.to_string()))
    }

    fn list(v: &[Atom]) -> Node {
        use std::collections::*;
        Node::Atom(Atom::List(v.iter().map(|x| x.clone()).collect::<VecDeque<Atom>>()))
    }
}

impl From<i64> for Node {
    fn from(i: i64) -> Node {
        Node::Atom(Atom::Number(Number::Integer(i)))
    }
}

impl From<f64> for Node {
    fn from(f: f64) -> Node {
        Node::Atom(Atom::Number(Number::Float(f)))
    }
}

impl From<bool> for Node {
    fn from(b: bool) -> Node {
        Node::Atom(Atom::Boolean(b))
    }
}

fn read_string(iter: &mut Peekable<Chars>) -> Result<Option<Token>, Error> {
    let mut s = String::new();
    loop {
        match iter.next() {
            Some('\\') => {
                if let Some(escaped) = iter.next() {
                    s.push(escaped);
                } else {
                    return Err(Error::Parser)
                }
            },
            Some('"') => {
                let t = Token::Atom(Atom::String(s));

                return Ok(Some(t))
            },
            Some(c) => s.push(c),
            None => return Err(Error::Parser)
        }
    }
}

fn read_atom(c: char, iter: &mut Peekable<Chars>) -> Result<Option<Token>,Error> {
    let mut s = String::new();
    s.push(c);
    loop {
        match iter.peek() {
            Some(&')') =>  {
                return Ok(Some(Token::Atom(Atom::parse(&s))))
            },
            _ => ()
        }

        if let Some(c) = iter.next() {
            if !c.is_whitespace() {
                s.push(c);
            } else {
                return Ok(Some(Token::Atom(Atom::parse(&s))))
            }
        } else {
            return Ok(Some(Token::Atom(Atom::parse(&s))))
        }
    }
}

fn next(iter: &mut Peekable<Chars>) -> Result<Option<Token>, Error> {
    loop {
        if let Some(c) = iter.next() {
            match c {
                '(' => return Ok(Some(Token::Open)),
                '"' => return read_string(iter),
                ')' => return Ok(Some(Token::Close)),
                '\'' => return Ok(Some(Token::Quote)),
                ';' =>  {
                    loop {
                        match iter.next() {
                            Some('\n') => break,
                            Some(_) => (),
                            None => return Ok(None)
                        }
                    }
                },
                _ if c.is_whitespace() => (),
                _ => return read_atom(c, iter)
            }
        } else {
            return Ok(None)
        }
    }
}

fn read_tokens(chars: &mut Peekable<Chars>) -> ParseResult {
    match next(chars) {
        Ok(Some(Token::Open)) => {
            let mut node: Vec<Node> = vec![];

            loop {
                match chars.peek() {
                    Some(&')') => {
                        chars.next();
                        break
                    },
                    Some(_) => {
                        let token = try!(read_tokens(chars));
                        node.push(token);
                    },
                    _ => {
                        return Err(Error::Parser)
                    }
                }
            }

            return Ok(Node::List(node))
        },
        Ok(Some(Token::Close)) => {
            return Err(Error::Parser)
        },
        Ok(Some(Token::Atom(x))) => return Ok(Node::Atom(x)),
        _ => ()
    }
    Err(Error::Parser)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn naked_atoms() {
        assert_eq!(Node::from(0), tokenize("0").unwrap());
        assert_eq!(Node::from(512), tokenize("512").unwrap());
        assert_eq!(Node::from(-512), tokenize("-512").unwrap());
        assert_eq!(Node::from(5.0f64), tokenize("5.0").unwrap());
        assert_eq!(Node::string("foo bar"), tokenize("\"foo bar\"").unwrap());
        assert_eq!(Node::symbol("foo"), tokenize("foo").unwrap());
    }

    #[test]
    fn string_escaping() {
        assert_eq!(Node::string("foo'bar"), tokenize("\"foo\\'bar\"").unwrap());
        assert_eq!(Node::string("foo\"bar"), tokenize("\"foo\\\"bar\"").unwrap());
    }
}
