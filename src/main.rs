extern crate regex;
extern crate readline;

use regex::Regex;
use std::collections::HashMap;
use std::io::prelude::*;
use std::iter::Iterator;
use std::string::String;

#[derive(Debug, PartialEq, Clone)]
enum Atom {
    Integer(i64),
    Float(f64),
    Symbol(String),
    Boolean(bool),
    Nil
}

type Function = Fn(&Vec<Atom>) -> AtomResult;
type FunctionMap = HashMap<String, Box<Function>>;

#[derive(Debug)]
enum Error {
    UnexpectedType,
    Parser,
    InvalidArguments,
    NotAFunction,
    NotImplemented
}

type AtomResult = Result<Atom, Error>;
type ParseResult = Result<Node, Error>;

fn nil_func(_: &Vec<Atom>) -> AtomResult {
    return Ok(Atom::Nil);
}

struct Env {
    func_map: FunctionMap,
    def_map: HashMap<String, Atom>
}

fn add(v: &Vec<Atom>) -> AtomResult {
    let mut result = 0;

    for i in v {
        if let Atom::Integer(d) =  *i {
            result += d
        } else {
            return Err(Error::UnexpectedType)
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

fn equals(args: &Vec<Atom>) -> AtomResult {
    let v = &args[0];

    for i in args.iter().skip(1) {
        if v != i {
            return Ok(Atom::Boolean(false));
        }
    }
    Ok(Atom::Boolean(true))
}

fn default_env() -> Env {
    let mut env = Env{func_map: HashMap::new(), def_map: HashMap::new()};

    env.func_map.insert("nil".to_string(), Box::new(nil_func) as Box<Function>);
    env.func_map.insert("+".to_string(), Box::new(add) as Box<Function>);
    env.func_map.insert("-".to_string(), Box::new(sub) as Box<Function>);
    env.func_map.insert("=".to_string(), Box::new(equals) as Box<Function>);

    return env;
}

fn tokenize(line: &str) -> ParseResult {
    let r = Regex::new(r"\s+").unwrap();
    let l = line.replace("(", " ( ").replace(")", " ) ").trim().to_string();
    let iter = r.split(&l);
    return read_tokens(&mut iter.peekable());
}

fn atom(token: &str) -> Atom {
    match token {
        "true" => return Atom::Boolean(true),
        "false" => return Atom::Boolean(false),
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

#[derive(Debug,PartialEq)]
enum Node {
    Atom(Atom),
    List(Vec<Node>)
}

fn read_tokens(iter: &mut std::iter::Peekable<regex::RegexSplits>) -> ParseResult {
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
        Some(x) => return Ok(Node::Atom(atom(x))),
        None => return Err(Error::Parser)
    }
    unreachable!();
}

fn define(args: Vec<Atom>, env: &mut Env) -> AtomResult {
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }

    if let Atom::Symbol(ref key) = args[0] {
        env.def_map.insert(key.clone(), args[1].clone());
        Ok(Atom::Nil)
    } else {
        Err(Error::InvalidArguments)
    }
}

fn get(args: Vec<Atom>, env: &Env) -> AtomResult {
    if args.len() != 1 {
        return Err(Error::InvalidArguments);
    }

    if let Atom::Symbol(ref s) = args[0] {
        if let Some(a) = env.def_map.get(s) {
            Ok(a.clone())
        } else {
            Ok(Atom::Nil)
        }
    } else {
        Err(Error::InvalidArguments)
    }
}

fn eval_args(list: &Vec<Node>, env: &mut Env) -> Result<Vec<Atom>, Error> {
    let mut args: Vec<Atom> = vec![];

    for i in list.iter().skip(1) {
        let atom = try!(eval(i, env));
        args.push(atom);
    }
    return Ok(args);
}

fn eval(node: &Node, env: &mut Env) -> AtomResult {
    match *node {
        Node::List(ref list) => {
            match list[0] {
                Node::Atom(ref atom) => {
                    if let &Atom::Symbol(ref s) = atom {
                        match s.as_ref() {
                            "define" => {
                                let args = try!(eval_args(&list, env));
                                return define(args, env);
                            },
                            "get" => {
                                return get( try!(eval_args(&list, env)), env);
                            },
                            "if" => {
                                let predicate = try!(eval(&list[1], env));

                                let truthy = match predicate {
                                    Atom::Boolean(b) => b,
                                    Atom::Nil => false,
                                    _ => true
                                };

                                if truthy {
                                    return eval(&list[2], env);
                                } else if list.len() > 2 {
                                    return eval(&list[3], env);
                                } else {
                                    return Ok(Atom::Boolean(false))
                                }
                            },
                            _ => ()
                        };

                        let args = try!(eval_args(&list, env));

                        if let Some(f) = env.func_map.get(s) {
                            f(&args)
                        } else {
                            Ok(Atom::Nil)
                        }
                    } else {
                        Err(Error::NotAFunction)
                    }
                },
                Node::List(_) => panic!("derp")
            }
        },
        Node::Atom(ref atom) => Ok(atom.clone())
    }
}

fn parse(line: &str) -> ParseResult {
    tokenize(line)
}

fn repl() {
    println!("Rust Lisp!");
    let mut env = default_env();

    loop {
        match readline::readline(">") {
            Some(s) => {
                if let Ok(p) = parse(&s) {
                    match eval(&p, &mut env) {
                        Ok(r) => println!("{:?}", r),
                        Err(e) => println!("Error in evaluation: {:?}", e)
                    }
                } else {
                    println!("Error in parsing");
                }
            },
            None => {
                println!("Exiting...");
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
    use super::default_env;
    use super::eval;

    fn teval(s: &str) -> Atom {
        eval(&parse(s).unwrap(), &mut default_env()).unwrap()
    }

    fn make_atom_node(s: &str) -> Node {
        return Node::Atom(atom(s));
    }

    #[test]
    fn atom_test() {
        assert_eq!(Atom::Integer(32), atom("32"));
        assert_eq!(Atom::Symbol("symbol".to_string()), atom("symbol"));
    }

    #[test]
    fn simple_read_tokens() {
        // let x = parse("(+ 1 2)").unwrap();

        // let l: Vec<Node> = match x {
        //     Node::List(l) => assert_eq!(3, l.len()),
        //     _ => assert!(false)
        // };

        // let xx : Vec<Node> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

        // for pair in xx.iter().zip(l.iter()) {
        //     assert_eq!(pair.0, pair.1);
        // }
    }

    #[test]
    fn nested_read_tokens() {
        let x = parse("(+ 1 (* 2 2))").unwrap();

        // assert_eq!( atom("+"), x.children[0].atom);
        // assert_eq!( atom("1"), x.children[1].atom);
        // assert_eq!( Atom::Nil, x.children[2].atom);

        // assert_eq!( 3, x.children[2].children.len());
        // assert_eq!( atom("*"), x.children[2].children[0].atom);
        // assert_eq!( atom("2"), x.children[2].children[1].atom);
        // assert_eq!( atom("2"), x.children[2].children[2].atom);
    }

    #[test]
    fn subexp_token_test() {
        let x = parse("(+ 1 (+ 2 3) 4)").unwrap();

        // assert_eq!( atom("+"), x.children[0].atom);
        // assert_eq!( atom("1"), x.children[1].atom);
        // assert_eq!( Atom::Nil, x.children[2].atom);

        // assert_eq!( 3, x.children[2].children.len());
        // assert_eq!( atom("+"), x.children[2].children[0].atom);
        // assert_eq!( atom("2"), x.children[2].children[1].atom);
        // assert_eq!( atom("3"), x.children[2].children[2].atom);

        // assert_eq!(atom("4"), x.children[3].atom);
    }

    #[test]
    fn if_special_form() {
        let x = eval(&parse("(if (= 1 1) true false)").unwrap(), &mut default_env()).unwrap();

        assert_eq!(Atom::Boolean(true), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&parse("(if (= 1 2) true false)").unwrap(), &mut default_env()).unwrap();

        assert_eq!(Atom::Boolean(false), x);
    }

    #[test]
    fn if_no_else() {
        let x = teval("(if (= 1 1) true)");

        assert_eq!(Atom::Boolean(true), x);
    }

    #[test]
    #[should_panic]
    fn unmatched_bracket() {
        parse("(+ 1").unwrap();
    }

    #[test]
    #[should_panic]
    fn eof() {
         parse("").unwrap();
    }
}
