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

#[derive(Debug, Clone, PartialEq)]
enum Eval {
    Atom(Atom),
    Node(Node),
    Proc(Procedure)
}

#[derive (Debug, Clone, PartialEq)]
struct Procedure {
    params: Vec<Atom>,
    body: Node,
}

fn eval_procedure(p: &Procedure, args: &Vec<Eval>, env: &mut Env) ->  EvalResult {
    try!(env.apply(&p.params, args));

    let res = eval(&p.body, env);

    env.pop();

    res
}

#[derive (Debug,Clone)]
struct Env {
    def_map: Vec<HashMap<String, Eval>>,
}

impl Env {
    fn find(&self, key: &str, i: usize) -> Option<&Eval> {
        let map = &self.def_map[i];

        let val = map.get(key);

        match val {
            Some(_) => val,
            None => {
                if i > 0 {
                    self.find(key, i - 1 )
                } else {
                    None
                }
            }
        }
    }

    fn apply(&mut self, params: &Vec<Atom>, args: &Vec<Eval>) -> Result<(),Error> {
        if params.len() != args.len() {
            return Err(InvalidArguments);
        }

        let mut m = HashMap::new();

        for p in params.iter().zip(args.iter()) {
            if let &Atom::Symbol(ref sym) = p.0 {
                m.insert(sym.clone(), p.1.clone());
            } else {
                return Err(InvalidArguments);
            }
        }

        self.def_map.push(m);

        Ok(())
    }

    fn pop(&mut self) {
        self.def_map.pop();
    }

    fn get(&self, key: &str) -> Option<&Eval> {
        self.find(key, self.def_map.len() - 1 )
    }

    fn set(&mut self, key: String, value: Eval) {
        if let Some(map) = self.def_map.last_mut() {
            map.insert(key, value);
        }
    }
}
fn add(v: &Vec<Eval>, env: &Env) -> EvalResult {
    let mut result = 0;

    for i in v {
        match *i {
            Eval::Atom(Atom::Integer(d)) => {
                result += d
            },
            Eval::Atom(Atom::Symbol(ref s)) => {
                let v = env.get(s);
                match v {
                    Some(&Eval::Atom(Atom::Integer(d))) => result += d,
                    _ => return Err(UnexpectedType)
                }

            },
            _ => return Err(Error::UnexpectedType)
        }
    }

    Ok(Eval::Atom(Atom::Integer(result)))
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

    Ok(Eval::Atom(Atom::Integer(result)))
}

fn equals(args: &Vec<Eval>, env: &Env) -> EvalResult {
    if let Some(v) = args.first() {
        for i in args.iter().skip(1) {
            match *i {
                Eval::Atom(Atom::Symbol(ref s)) => {
                    match env.get(s) {
                        Some(d) => {
                            if d != v {
                                return Ok(Eval::Atom(Atom::Boolean(false)));
                            }
                        },
                        None => {
                            if v != &Eval::Atom(Atom::Nil) {
                                return Ok(Eval::Atom(Atom::Boolean(false)));
                            }
                        }
                    }
                }
                _ => {
                    if v != i {
                        return Ok(Eval::Atom(Atom::Boolean(false)))
                    }
                }
            }
        }
        Ok(Eval::Atom(Atom::Boolean(true)))
    } else {
        Err(InvalidArguments)
    }
}

fn default_env() -> Env {
    Env{def_map: vec![HashMap::new()]}
}

fn try_built_ins(sym: &str, args: &Vec<Eval>, env: &Env) -> Option<EvalResult> {
    match sym {
        "nil" => Some(Ok(Eval::Atom(Atom::Nil))),
        "+" => Some(add(args, env)),
        "-" => Some(sub(args)),
        "=" => Some(equals(args, env)),
        _ => None
    }
}

fn define(args: Vec<Eval>, env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }
    if let Eval::Atom(Atom::Symbol(ref key)) = args[0] {
        env.set(key.clone(), args[1].clone());
        Ok(Eval::Atom(Atom::Nil))
    } else {
        Err(Error::InvalidArguments)
    }
}

fn set(args: Vec<Eval>, env: &mut Env) -> EvalResult {
    // FIXME: get rid of the clones
    if args.len() != 2 {
        return Err(InvalidArguments)
    }

    if let Eval::Atom(Atom::Symbol(ref key)) = args[0] {
        // if env.def_map.contains_key(key) {
        //     let entry = env.def_map.entry((*key).clone()).or_insert(Eval::Atom(Atom::Nil));
        //     *entry = args[1].clone();
        //     return Ok(args[1].clone())
        // }
    }
    Err(InvalidArguments)
}

fn get(args: Vec<Eval>, env: &Env) -> EvalResult {
    if args.len() != 1 {
        return Err(Error::InvalidArguments)
    }

    if let Eval::Atom(Atom::Symbol(ref s)) = args[0] {
        if let Some(a) = env.get(s) {
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
    Ok(args)
}

fn quote(v: &Vec<Node>) -> Result<Eval, Error> {
    Ok(Eval::Node(v[1].clone()))
}

fn reduce_fn_params(l: &Vec<Node>) -> Result<Vec<Atom>,Error> {
    let mut v = vec![];

    for node in l {
        match node {
            &Node::Atom(ref a) => v.push(a.clone()),
            _ => {
                return Err(InvalidArguments)
            }
        }
    }
    Ok(v)
}

fn collect_body(l: &Vec<Node>) -> Vec<Node> {
    let mut v = vec![];

    for i in l.iter().skip(2) {
        v.push(i.clone());
    }
    v
}

fn eval_node(atom: &Atom, list: &Vec<Node>, env: &mut Env) -> EvalResult {
    match atom {
        &Atom::Symbol(ref s) =>  {
            match s.as_ref() {
                "def" => {
                    let args = try!(eval_args(&list, env));
                    return define(args, env);
                },
                "fn" => {
                    let params = match list[1] {
                        Node::List(ref l) => try!(reduce_fn_params(l)),
                        _ => {
                            return Err(InvalidArguments)
                        }
                    };

                    let prc = Procedure{ params: params,
                                         body: Node::List(collect_body(list))
                    };

                    return Ok(Eval::Proc(prc))
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

            if let Some(r) = try_built_ins(&s, &args, env) {
                return r
            }

            if let Some(&Eval::Proc(ref p)) = env.get(&s) {
                let mut renv = env.clone();
                let r = eval_procedure(p, &args, &mut renv);
                return r
            }

            Ok(Eval::Atom(Atom::Nil))
        },
        _ => {
            Ok(Eval::Atom(atom.clone()))
        }
    }
}

fn eval(node: &Node, env: &mut Env) -> Result<Eval, Error> {
    match node {
        &Node::List(ref list) => {
            match list[0] {
                Node::Atom(ref atom) => {
                    return eval_node(atom, list, env)
               },
                Node::List(_) => {
                    let r = try!(eval(&list[0], env));

                    match r {
                        Eval::Atom(a) => return eval_node(&a, list, env),
                        Eval::Proc(p) => return eval_procedure(&p, &try!(eval_args(&list, env)), env),
                        _ => {
                            return Err(InvalidArguments)
                        }
                    }
                }
            }
        },
        &Node::Atom(ref atom) => {
            match atom {
                &Atom::Symbol(ref s) => {
                    match env.get(s) {
                        Some(e) => return Ok(e.clone()),
                        _ => return Ok(Eval::Atom(atom.clone()))
                    }
                }
                _ => Ok(Eval::Atom(atom.clone()))
            }
        }
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
    use super::Eval;

    fn teval(s: &str) -> Eval {
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

        assert_eq!(Eval::Atom(Atom::Boolean(true)), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&tokenize("(if (= 1 2) true false)").unwrap(), &mut default_env()).unwrap();

        assert_eq!(Eval::Atom(Atom::Boolean(false)), x);
    }

    #[test]
    fn if_no_else() {
        let x = teval("(if (= 1 1) true)");

        assert_eq!(Eval::Atom(Atom::Boolean(true)), x);
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
