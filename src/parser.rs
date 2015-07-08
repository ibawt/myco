use atom::*;
use errors::*;
use number::*;
use std::iter::*;
use std::io::prelude::*;

pub fn tokenize(line: &str) -> ParseResult {
    let mut chars = line.chars().peekable();
    return read_tokens(&mut chars);
}

pub type ParseResult = Result<Atom, Error>;

use std::fmt;

// impl fmt::Display for Node {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         match *self {
//             Node(ref node) => {
//                 write!(f, "{}", node)
//             },
//             Quote(ref node) => {
//                 try!(write!(f, "'"));
//                 write!(f, "{}", node)
//             },
//             QuasiQuote(ref node) => {
//                 try!(write!(f, "`"));
//                 write!(f, "{}", node)
//             },

//             Splice(ref node) => {
//                 try!(write!(f, "~@"));
//                 write!(f, "{}", node)
//             },
//             Unquote(ref node) => {
//                 write!(f, "~{}", node)
//             }
//         }
//     }
// }

// impl fmt::Display for Node {
//     fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
//         use self::Node::*;
//         match *self {
//             Atom(ref a) => {
//                 write!(f, "{}", a)
//             },
//             List(ref list) => {
//                 try!(write!(f, "("));
//                 if let Some(first) = list.first() {
//                     try!(write!(f, "{}", first));

//                     for n in list.iter().skip(1) {
//                         try!(write!(f, " {}", n));
//                     }
//                 }
//                 write!(f, ")")
//             }
//         }
//     }
// }

use std::str::Chars;

#[derive (Debug, PartialEq)]
enum Token {
    Atom(Atom),
    Open,        // (
    Close,       // )
    Quote,       // '
    QuasiQuote, // `
    Unquote,     // ~
    Splice,      // ~@
}

use std::convert::From;

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
            let mut node = List::new();

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

            Ok(Atom::List(node))
        },
        Ok(Some(Token::Close)) => {
            Err(Error::Parser)
        },
        Ok(Some(Token::Quote)) => {
            let mut list = List::with_capacity(2);
            list.push(Atom::Form(Form::Quote));
            list.push(try!(read_tokens(chars)));
            Ok(Atom::List(list))
        },
        Ok(Some(Token::QuasiQuote)) => {
            // if let Node::Node(node) = try!(read_tokens(chars)) {
            //     return Ok(Node::QuasiQuote(node))
            // }
            // return Err(Error::Parser)
            Err(Error::NotImplemented)
        }
        Ok(Some(Token::Unquote)) => {
            // if let Node::Node(node) = try!(read_tokens(chars)) {
            //     return Ok(Node::Unquote(node))
            // }
            Err(Error::Parser)
        },
        Ok(Some(Token::Splice)) => {
            // if let Node::Node(node) = try!(read_tokens(chars)) {
            //     return Ok(Node::Splice(node))
            // }
            Err(Error::NotImplemented)
        }
        Ok(Some(Token::Atom(x))) => return Ok(x),
        _ => Err(Error::Parser)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use atom::*;

    fn make_atom_node(s: &str) -> Atom {
        Atom::parse(s)
    }

    fn as_list(n: &Atom) -> &[Atom] {
        match *n {
            Atom::List(ref l) => l,
            _ => panic!("don't get here!")
        }
    }

    fn as_atom(n: &Atom) -> &Atom {
        n
    }

    fn atom(s: &str) -> Atom {
        Atom::parse(s)
    }


    #[test]
    fn naked_atoms() {
        assert_eq!(Atom::from(0), tokenize("0").unwrap());
        assert_eq!(Atom::from(512), tokenize("512").unwrap());
        assert_eq!(Atom::from(-512), tokenize("-512").unwrap());
        assert_eq!(Atom::from(5.0f64), tokenize("5.0").unwrap());
        assert_eq!(Atom::string("foo bar"), tokenize("\"foo bar\"").unwrap());
        assert_eq!(Atom::symbol("foo"), tokenize("foo").unwrap());
    }

    #[test]
    fn string_escaping() {
        assert_eq!(Atom::string("foo'bar"), tokenize("\"foo\\'bar\"").unwrap());
        assert_eq!(Atom::string("foo\"bar"), tokenize("\"foo\\\"bar\"").unwrap());
    }

    #[test]
    fn simple_read_tokens() {
        let x = tokenize("(+ 1 2)").unwrap();

        let l = as_list(&x);

        assert_eq!(3, l.len());

        let xx : Vec<Atom> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

        for pair in xx.iter().zip(l.iter()) {
            assert_eq!(pair.0, pair.1);
        }
    }

    #[test]
    fn nested_read_tokens() {
        let x = tokenize("(+ 1 (* 2 2))").unwrap();

        let l = as_list(&x);

        assert_eq!( atom("+"), *as_atom(&l[0]));
        assert_eq!( atom("1"), *as_atom(&l[1]));

        let l2 = as_list(&l[2]);

        assert_eq!( 3, l.len());
        assert_eq!( atom("*"), *as_atom(&l2[0]));
        assert_eq!( atom("2"), *as_atom(&l2[1]));
        assert_eq!( atom("2"), *as_atom(&l2[2]));
    }

    #[test]
    fn subexp_token_test() {
        let x = tokenize("(+ 1 (+ 2 3) 4)").unwrap();

        let l = as_list(&x);

        assert_eq!( atom("+"), *as_atom(&l[0]));
        assert_eq!( atom("1"), *as_atom(&l[1]));

        let l2 = as_list(&l[2]);

        assert_eq!( 3, l2.len());
        assert_eq!( atom("+"), *as_atom(&l2[0]));
        assert_eq!( atom("2"), *as_atom(&l2[1]));
        assert_eq!( atom("3"), *as_atom(&l2[2]));

        assert_eq!(atom("4"), *as_atom(&l[3]));
    }

    #[test]
    #[should_panic]
    fn unmatched_bracket() {
        tokenize("(+ 1").unwrap();
    }

    #[test]
    #[should_panic]
    fn eof() {
         tokenize("").unwrap();
    }
}
