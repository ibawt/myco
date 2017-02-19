use atom::*;
use errors::*;
use std::iter::*;

pub fn tokenize(line: &str) -> ParseResult {
    let mut chars = line.chars().peekable();

    let mut v = vec![Atom::Form(Form::Do)];

    while let Some(_) = chars.peek() {
        if let Ok(a) = read_tokens(&mut chars) {
            v.push(a);
        }
    }

    if v.len() == 1 {
        return Err(Error::EoF);
    }

    Ok(Atom::list(v))
}

pub fn tokenize_single(line: &str) -> ParseResult {
    let mut chars = line.chars().peekable();

    while let Some(_) = chars.peek() {
        if let Ok(a) = read_tokens(&mut chars) {
            return Ok(a)
        }
    }
    Err(Error::EoF)
}

pub type ParseResult = Result<Atom, Error>;

use std::str::Chars;

#[derive (Debug, PartialEq)]
enum Token {
    Atom(Atom),
    Open, // (
    Close, // )
    Quote, // '
    QuasiQuote, // `
    Unquote, // ~
    Splice, // ~@
}

fn read_string(iter: &mut Peekable<Chars>) -> Result<Option<Token>, Error> {
    let mut s = String::new();
    loop {
        match iter.next() {
            Some('\\') => {
                if let Some(escaped) = iter.next() {
                    s.push(escaped);
                } else {
                    return Err(Error::EoF);
                }
            }
            Some('"') => {
                let t = Token::Atom(Atom::String(s));

                return Ok(Some(t));
            }
            Some(c) => s.push(c),
            None => return Err(Error::EoF),
        }
    }
}

fn read_atom(c: char, iter: &mut Peekable<Chars>) -> Result<Option<Token>, Error> {
    let mut s = String::new();
    s.push(c);
    loop {
        if let Some(&')') = iter.peek() {
            return Ok(Some(Token::Atom(Atom::parse(&s))));
        }

        if let Some(c) = iter.next() {
            if !c.is_whitespace() {
                s.push(c);
            } else {
                return Ok(Some(Token::Atom(Atom::parse(&s))));
            }
        } else {
            return Ok(Some(Token::Atom(Atom::parse(&s))));
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
                '`' => return Ok(Some(Token::QuasiQuote)),
                '~' => {
                    match *try!(iter.peek().ok_or(Error::EoF)) {
                        '@' => {
                            iter.next();
                            return Ok(Some(Token::Splice));
                        }
                        _ => return Ok(Some(Token::Unquote)),
                    }
                }
                ';' => {
                    loop {
                        match iter.next() {
                            Some('\n') => break,
                            Some(_) => (),
                            None => return Ok(None),
                        }
                    }
                }
                _ if c.is_whitespace() => (),
                _ => return read_atom(c, iter),
            }
        } else {
            return Ok(None);
        }
    }
}

fn make_quote_form(f: Form, chars: &mut Peekable<Chars>) -> ParseResult {
    let mut list = Vec::with_capacity(2);
    list.push(Atom::Form(f));
    list.push(try!(read_tokens(chars)));
    Ok(Atom::list(list))
}

fn read_tokens(chars: &mut Peekable<Chars>) -> ParseResult {
    let token = try!(try!(next(chars)).ok_or(Error::EoF));

    match token {
        Token::Open => {
            let mut node = Vec::new();

            loop {
                match chars.peek() {
                    Some(&')') => {
                        chars.next();
                        break;
                    }
                    None => return Err(Error::EoF),
                    _ => {
                        let token = try!(read_tokens(chars));
                        node.push(token);
                    }
                }
            }

            Ok(Atom::list(node))
        }
        Token::Close => Err(Error::Parser),
        Token::Quote => make_quote_form(Form::Quote, chars),
        Token::QuasiQuote => make_quote_form(Form::QuasiQuote, chars),
        Token::Unquote => make_quote_form(Form::Unquote, chars),
        Token::Splice => make_quote_form(Form::Splice, chars),
        Token::Atom(x) => Ok(x),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn make_atom_node(s: &str) -> Atom {
        Atom::parse(s)
    }

    fn as_list(n: &Atom) -> &[Atom] {
        match *n {
            Atom::List(ref l) => l,
            _ => panic!("don't get here!"),
        }
    }

    fn as_atom(n: &Atom) -> &Atom {
        n
    }

    fn atom(s: &str) -> Atom {
        Atom::parse(s)
    }

    fn second(a: Atom) -> Atom {
        match a {
            Atom::List(ref l) => l[1].clone(),
            _ => panic!(),
        }
    }


    #[test]
    fn naked_atoms() {
        assert_eq!(Atom::from(0), second(tokenize("0").unwrap()));
        assert_eq!(Atom::from(512), second(tokenize("512").unwrap()));
        assert_eq!(Atom::from(-512), second(tokenize("-512").unwrap()));
        assert_eq!(Atom::from(5.0f64), second(tokenize("5.0").unwrap()));
        assert_eq!(Atom::string("foo bar"),
                   second(tokenize("\"foo bar\"").unwrap()));
        assert_eq!(Atom::symbol("foo"), second(tokenize("foo").unwrap()));
    }

    #[test]
    fn string_escaping() {
        assert_eq!(Atom::string("foo'bar"),
                   second(tokenize("\"foo\\'bar\"").unwrap()));
        assert_eq!(Atom::string("foo\"bar"),
                   second(tokenize("\"foo\\\"bar\"").unwrap()));
    }

    #[test]
    fn simple_read_tokens() {
        let x = second(tokenize("(+ 1 2)").unwrap());

        let l = as_list(&x);

        assert_eq!(3, l.len());

        let xx: Vec<Atom> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

        for pair in xx.iter().zip(l.iter()) {
            assert_eq!(pair.0, pair.1);
        }
    }

    #[test]
    fn nested_read_tokens() {
        let x = second(tokenize("(+ 1 (* 2 2))").unwrap());

        let l = as_list(&x);

        assert_eq!(atom("+"), *as_atom(&l[0]));
        assert_eq!(atom("1"), *as_atom(&l[1]));

        let l2 = as_list(&l[2]);

        assert_eq!(3, l.len());
        assert_eq!(atom("*"), *as_atom(&l2[0]));
        assert_eq!(atom("2"), *as_atom(&l2[1]));
        assert_eq!(atom("2"), *as_atom(&l2[2]));
    }

    #[test]
    fn subexp_token_test() {
        let x = second(tokenize("(+ 1 (+ 2 3) 4)").unwrap());

        let l = as_list(&x);

        assert_eq!(atom("+"), *as_atom(&l[0]));
        assert_eq!(atom("1"), *as_atom(&l[1]));

        let l2 = as_list(&l[2]);

        assert_eq!(3, l2.len());
        assert_eq!(atom("+"), *as_atom(&l2[0]));
        assert_eq!(atom("2"), *as_atom(&l2[1]));
        assert_eq!(atom("3"), *as_atom(&l2[2]));

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

    #[test]
    fn incomplete_expression() {
        assert_eq!(tokenize("(if foo"), Err(Error::EoF));
    }
}
