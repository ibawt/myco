extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::io::prelude::*;
use std::io::stdin;
use std::iter::Iterator;
use std::string::String;

#[derive(Debug, PartialEq, Clone)]
enum Atom {
    Integer(i64),
    Float(f64),
    Symbol(String),
    Nil
}

type Function = Fn(&Vec<Atom>) -> AtomResult;
type FunctionMap = HashMap<String, Box<Function>>;

#[derive(Debug)]
enum Error {
    UnexpectedType,
    Parser,
    InvalidArguments
}

type AtomResult = Result<Atom, Error>;
type ParseResult = Result<Node, Error>;

fn nil_func(_: &Vec<Atom>) -> AtomResult {
    return Ok(Atom::Nil);
}

struct Env {
    func_map: FunctionMap
}

fn add(v: &Vec<Atom>) -> AtomResult {
    let mut result = 0;

    for i in v {
        match *i {
            Atom::Integer(d) => result += d,
            _ => return Err(Error::UnexpectedType)
        }
    }

    return Ok(Atom::Integer(result));
}

fn sub(v: &Vec<Atom>) -> AtomResult {
    if v.len() < 1 {
        return Err(Error::InvalidArguments)
    }

    let mut result = match v[0] {
        Atom::Integer(d) => d,
        _ => return Err(Error::UnexpectedType)
    };

    for i in v.iter().skip(1) {
        match *i {
            Atom::Integer(d) => result -= d,
            _ => return Err(Error::UnexpectedType)
        }
    }

    return Ok(Atom::Integer(result));
}


fn default_env() -> Env {
    let mut env = Env{func_map: HashMap::new()};

    env.func_map.insert("nil".to_string(), Box::new(nil_func) as Box<Function>);
    env.func_map.insert("+".to_string(), Box::new(add) as Box<Function>);
    env.func_map.insert("-".to_string(), Box::new(sub) as Box<Function>);

    return env;
}

fn tokenize(line: &str) -> ParseResult {
    let r = Regex::new(r"\s+").unwrap();
    let l = line.replace("(", " ( ").replace(")", " ) ").trim().to_string();
    let iter = r.split(&l);
    return read_tokens(&mut iter.peekable());
}

fn atom(token: &str) -> Atom {
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

#[derive(Debug,PartialEq)]
struct Node {
    atom: Atom,
    children: Vec<Node>
}

fn read_tokens(iter: &mut std::iter::Peekable<regex::RegexSplits>) -> ParseResult {
    let mut node = Node{ atom: Atom::Nil, children: vec![] };

    match iter.next() {
        Some("(") => {
            loop {
                match iter.peek() {
                    Some(&")") => break,
                    Some(_) => {},
                    None => return Err(Error::Parser)
                }
                let token = try!(read_tokens(iter));
                node.children.push(token);
            }
        },
        Some(")") => return Err(Error::Parser),
        Some(x) => {
            node.atom = atom(x);
        },
        None => return Err(Error::Parser)
    }
    return Ok(node);
}

fn eval(node: &Node, env: &mut Env) -> AtomResult {
    match node.atom {
        Atom::Nil => {
            // TODO: use the iterators
            let mut args: Vec<Atom> = vec![];
            for i in node.children.iter().skip(1) {
                let a = try!(eval(i, env));
                args.push(a);
            }

            match node.children[0].atom {
                Atom::Symbol(ref s) => {
                    if *s == "define" {
                        println!("define!");
                        return Ok(Atom::Nil);
                    }

                    match env.func_map.get(s) {
                        Some(f) => return f(&args),
                        None => return Ok(Atom::Nil)
                    }
                },
                _ => unreachable!()
            }
        },
        Atom::Integer(i) => {
            println!("an integer: {}", i);
        },
        Atom::Float(f) => {
            println!("a float: {}", f);
        },
        Atom::Symbol(ref s) => {
            println!("a symbol, {}", s);
        }
    }

    return Ok(node.atom.clone());
}

fn parse(line: &str) -> ParseResult {
    tokenize(line)
}

fn flush() {
    let out = std::io::stdout();
    let mut o = out.lock();
    o.flush().unwrap();
}

fn repl() {
    println!("Rust Lisp!");
    let mut input = stdin();
    let mut line = String::new();
    let mut env = default_env();

    loop {
        print!(">");
        flush();

        match input.read_line(&mut line) {
            Ok(size) => {
                if size == 0 {
                    println!("Bye!");
                    break;
                } else {
                    if let Ok(p) = parse(&line) {
                        match eval(&p, &mut env) {
                            Ok(r) => println!("{:?}", r),
                            Err(e) => println!("Error in evaluation: {:?}", e)
                        }
                    } else {
                        println!("Error in parsing");
                    }
                }
            },
            Err(e) => {
                println!("Error: {:?}", e);
                break;
            }
        }
    }
}

pub fn main() {
    repl();
}

#[cfg(test)]
mod test {
    use super::Node;
    use super::Atom;
    use super::atom;
    use super::parse;

    fn make_atom_node(s: &str) -> Node {
        return Node{ atom: atom(s), children: vec![]};
    }

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Integer(32), atom("32"));
        assert_eq!(Atom::Symbol("symbol".to_string()), atom("symbol"));
    }

    #[test]
    fn simple_read_tokens() {
        let x = parse("(+ 1 2)");

        assert_eq!(Atom::Nil, x.atom);
        assert_eq!(3, x.children.len());

        let xx : Vec<Node> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

        for pair in xx.iter().zip(x.children.iter()) {
            assert_eq!(pair.0, pair.1);
        }
    }

    #[test]
    fn nested_read_tokens() {
        let x = parse("(+ 1 (* 2 2))");

        assert_eq!( atom("+"), x.children[0].atom);
        assert_eq!( atom("1"), x.children[1].atom);
        assert_eq!( Atom::Nil, x.children[2].atom);

        assert_eq!( 3, x.children[2].children.len());
        assert_eq!( atom("*"), x.children[2].children[0].atom);
        assert_eq!( atom("2"), x.children[2].children[1].atom);
        assert_eq!( atom("2"), x.children[2].children[2].atom);
    }

    #[test]
    #[should_panic]
    fn unmatched_bracket() {
        let x = parse("(+ 1");
    }

    #[test]
    #[should_panic]
    fn eof() {
        let x = parse("");
    }
}
