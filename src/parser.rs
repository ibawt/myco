extern crate regex;

use atom::*;
use errors::*;

use std::iter::*;
use regex::Regex;

// struct Scanner {
//     buf: String,
//     pos: usize
// }

// impl Scanner {
//     fn next(&mut self) -> &str {
//         ""
//     }
// }

pub fn tokenize(line: &str) -> ParseResult {
    let r = Regex::new(r"\s+").unwrap();
    let l = line.replace("(", " ( ").replace(")", " ) ").trim().to_string();
    let iter = r.split(&l);
    return read_tokens(&mut iter.peekable());
}

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Atom(Atom),
    List(Vec<Node>)
}

pub type ParseResult = Result<Node, Error>;

fn read_tokens(iter: &mut Peekable<regex::RegexSplits>) -> ParseResult {
    match iter.next() {
        Some("(") => {
            let mut node: Vec<Node> = vec![];

            loop {
                match iter.peek() {
                    Some(&")") => {
                        iter.next();
                        break
                    },
                    Some(_) => {
                        let token = try!(read_tokens(iter));
                        node.push(token);
                    },
                    None => return Err(Error::Parser)
                }
            }
            return Ok(Node::List(node));
        },
        Some(")") => return Err(Error::Parser),
        Some(x) => return Ok(Node::Atom(Atom::parse(x))),
        _ => ()
    }
    return Err(Error::Parser);
}
