extern crate regex;
extern crate readline;

mod errors;
mod atom;
mod parser;

use parser::*;
use atom::*;
use errors::Error;
use errors::Error::*;

use std::collections::HashMap;
use std::string::String;
type EvalResult = Result<Eval, Error>;
type Function = Fn(&Vec<Eval>) -> EvalResult;
type FunctionMap = HashMap<String, Box<Function>>;

fn nil_func(_: &Vec<Eval>) -> EvalResult {
    return Ok(Eval::Atom(Atom::Nil));
}

struct Env {
    func_map: FunctionMap,
    def_map: HashMap<String, Eval>
}

fn add(v: &Vec<Eval>) -> EvalResult {
    let mut result = 0;

    for i in v {
        if let Eval::Atom(Atom::Integer(d)) = *i {
                result += d
        } else {
            return Err(Error::UnexpectedType);
        }
    }

    return Ok(Eval::Atom(Atom::Integer(result)));
}

fn sub(v: &Vec<Eval>) -> EvalResult {
    if v.len() < 1 {
        return Err(Error::InvalidArguments)
    }

    let mut result = match v[0] {
        Eval::Atom(Atom::Integer(d)) => d,
        _ => return Err(Error::UnexpectedType)
    };

    for i in v.iter().skip(1) {
        match *i {
            Eval::Atom(Atom::Integer(d)) => result -= d,
            _ => return Err(Error::UnexpectedType)
        }
    }

    return Ok(Eval::Atom(Atom::Integer(result)));
}

fn equals(args: &Vec<Eval>) -> EvalResult {
    let v = &args[0];

    for i in args.iter().skip(1) {
        if v != i {
            return Ok(Eval::Atom(Atom::Boolean(false)));
        }
    }
    Ok(Eval::Atom(Atom::Boolean(true)))
}

fn default_env() -> Env {
    let mut env = Env{func_map: HashMap::new(), def_map: HashMap::new()};

    env.func_map.insert("nil".to_string(), Box::new(nil_func) as Box<Function>);
    env.func_map.insert("+".to_string(), Box::new(add) as Box<Function>);
    env.func_map.insert("-".to_string(), Box::new(sub) as Box<Function>);
    env.func_map.insert("=".to_string(), Box::new(equals) as Box<Function>);

    return env;
}

fn define(args: Vec<Eval>, env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }

    if let Eval::Atom(Atom::Symbol(ref key)) = args[0] {
        env.def_map.insert(key.clone(), args[1].clone());
        Ok(Eval::Atom(Atom::Nil))
    } else {
        Err(Error::InvalidArguments)
    }
}

fn set(args: Vec<Eval>, env: &mut Env) -> EvalResult {
    // FIXME: get rid of the clones
    if args.len() != 2 {
        return Err(InvalidArguments);
    }

    if let Eval::Atom(Atom::Symbol(ref key)) = args[0] {
        if env.def_map.contains_key(key) {
            let entry = env.def_map.entry((*key).clone()).or_insert(Eval::Atom(Atom::Nil));
            *entry = args[1].clone();
            return Ok(args[1].clone())
        }
    }
    Err(InvalidArguments)
}

fn get(args: Vec<Eval>, env: &Env) -> EvalResult {
    if args.len() != 1 {
        return Err(Error::InvalidArguments);
    }

    if let Eval::Atom(Atom::Symbol(ref s)) = args[0] {
        if let Some(a) = env.def_map.get(s) {
            Ok(a.clone())
        } else {
            Ok(Eval::Atom(Atom::Nil))
        }
    } else {
        Err(Error::InvalidArguments)
    }
}

fn eval_args(list: &Vec<Node>, env: &mut Env) -> Result<Vec<Eval>, Error> {
    let mut args = vec![];

    for i in list.iter().skip(1) {
        let atom = try!(eval(i, env));
        args.push(atom);
    }
    return Ok(args);
}
#[derive(Debug, Clone, PartialEq)]
enum Eval {
    Atom(Atom),
    Node(Node)
}

fn quote(v: &Vec<Node>) -> Result<Eval, Error> {
    return Ok(Eval::Node(v[1].clone()));
}

fn eval(node: &Node, env: &mut Env) -> Result<Eval, Error> {
    match node {
        &Node::List(ref list) => {
            match list[0] {
                Node::Atom(ref atom) => {
                    if let &Atom::Symbol(ref s) = atom {
                        match s.as_ref() {
                            "def" => {
                                let args = try!(eval_args(&list, env));
                                return define(args, env);
                            },
                            "quote" => {
                                return quote(&list);
                            },
                            "set!" => {
                                return set( try!(eval_args(&list, env)), env);
                            },
                            "get" => {
                                return get( try!(eval_args(&list, env)), env);
                            },
                            "if" => {
                                let predicate = try!(eval(&list[1], env));

                                let truthy = match predicate {
                                    Eval::Atom(Atom::Boolean(b)) => b,
                                    Eval::Atom(Atom::Nil) => false,
                                    _ => true
                                };

                                if truthy {
                                    return eval(&list[2], env);
                                } else if list.len() > 2 {
                                    return eval(&list[3], env);
                                } else {
                                    return Ok(Eval::Atom(Atom::Boolean(false)))
                                }
                            },
                            _ => ()
                        };

                        let args = try!(eval_args(&list, env));
                        if let Some(f) = env.func_map.get(s) {
                            f(&args)
                        } else {
                            Ok(Eval::Atom(Atom::Nil))
                        }
                    } else {
                        Err(Error::NotAFunction)
                    }
                },
                Node::List(_) => panic!("derp")
            }
        },
        &Node::Atom(ref atom) => Ok(Eval::Atom(atom.clone()))
    }
}

fn repl() {
    println!("Rust Lisp!");
    let mut env = default_env();

    loop {
        match readline::readline(">") {
            Some(s) => {
                if let Ok(p) = parser::tokenize(&s) {
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
    use parser::*;
    use super::default_env;
    use super::eval;
    use atom::Atom;

    fn teval(s: &str) -> Atom {
        eval(&tokenize(s).unwrap(), &mut default_env()).unwrap()
    }

    fn make_atom_node(s: &str) -> Node {
        return Node::Atom(Atom::parse(s));
    }

    fn as_list(n: &Node) -> &Vec<Node> {
        match n {
            &Node::List(ref l) => l,
            _ => panic!("don't get here!")
        }
    }

    fn as_atom(n: &Node) -> &Atom {
        match n {
            &Node::Atom(ref a) => a,
            _ => panic!("not here!")
        }
    }

    fn atom(s: &str) -> Atom {
        Atom::parse(s)
    }

    #[test]
    fn simple_read_tokens() {
        let x = tokenize("(+ 1 2)").unwrap();

        let l = as_list(&x);

        assert_eq!(3, l.len());

        let xx : Vec<Node> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

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
    fn if_special_form() {
        let x = eval(&tokenize("(if (= 1 1) true false)").unwrap(), &mut default_env()).unwrap();

        assert_eq!(Atom::Boolean(true), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&tokenize("(if (= 1 2) true false)").unwrap(), &mut default_env()).unwrap();

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
        tokenize("(+ 1").unwrap();
    }

    #[test]
    #[should_panic]
    fn eof() {
         tokenize("").unwrap();
    }
}
