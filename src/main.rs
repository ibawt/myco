extern crate regex;
extern crate readline;

mod errors;
mod number;
mod atom;
mod parser;

use number::*;
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

use std::fmt;
impl fmt::Display for Eval {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Eval::*;
        match *self {
            Atom(ref a) => write!(f, "{}", a),
            Node(ref n) => write!(f, "{}", n),
            Proc(ref p) => write!(f, "procedure")
        }
    }
}

#[derive (Debug, Clone, PartialEq)]
struct Procedure {
    params: Vec<Atom>,
    body: Node,
}

fn eval_procedure(p: &Procedure, args: &[Eval], env: &mut Env) ->  EvalResult {
    try!(env.apply(&p.params, args));

    let body = SyntaxNode::Node(p.body.clone());
    let res = eval(&body, env);

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

    fn apply(&mut self, params: &[Atom], args: &[Eval]) -> Result<(),Error> {
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
fn add(v: &[Eval], env: &Env) -> EvalResult {
    let mut result = Number::Integer(0);

    for i in resolve_symbols(v, env).iter() {
        match i {
            &Eval::Atom(Atom::Number(d)) => {
                result = result + d;
            },
            _ => return Err(Error::UnexpectedType)
        }
    }

    Ok(Eval::Atom(Atom::Number(result)))
}

enum Comparison {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual
}

use Comparison::*;

fn cmp(v: &[Eval], env: &Env, cmp: Comparison) -> EvalResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = resolve_symbols(v, env);

    let initial = match vv[0] {
        Eval::Atom(Atom::Number(a)) => a,
        _ => return Err(UnexpectedType)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Eval::Atom(Atom::Number(d)) => {
                let b = match cmp {
                    LessThan => initial < d,
                    GreaterThan => initial > d,
                    LessThanOrEqual => initial <= d,
                    GreaterThanOrEqual => initial >= d
                };

                if !b {
                    return Ok(Eval::Atom(Atom::Boolean(false)))
                }
            },
            _ => return Err(UnexpectedType)
        }
    }

    Ok(Eval::Atom(Atom::Boolean(true)))
}

fn sub(v: &[Eval], env: &Env) -> EvalResult {
    if v.len() < 1 {
        return Err(Error::InvalidArguments)
    }

    let vv = resolve_symbols(v, env);

    let mut result = match vv[0] {
        Eval::Atom(Atom::Number(d)) => d,
        _ => return Err(Error::UnexpectedType)
    };

    for i in vv.iter().skip(1) {
        match *i {
            Eval::Atom(Atom::Number(d)) => result = result - d,
            _ => return Err(Error::UnexpectedType)
        }
    }

    Ok(Eval::Atom(Atom::Number(result)))
}

fn mul(v: &[Eval], env: &Env) -> EvalResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = resolve_symbols(v, env);

    let mut initial = match vv[0] {
        Eval::Atom(Atom::Number(d)) => d,
        _ => return Err(InvalidArguments)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Eval::Atom(Atom::Number(d)) => initial = initial * d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Eval::Atom(Atom::Number(initial)))
}

fn div(v: &[Eval], env: &Env) -> EvalResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = resolve_symbols(v, env);

    let mut initial = match vv[0] {
        Eval::Atom(Atom::Number(d)) => d,
        _ => return Err(InvalidArguments)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Eval::Atom(Atom::Number(d)) => initial = initial / d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Eval::Atom(Atom::Number(initial)))
}

fn resolve_symbols(v: &[Eval], env: &Env) -> Vec<Eval> {
    v.iter()
        .map(|x| match x {
            &Eval::Atom(Atom::Symbol(ref s)) => {
                match env.get(s) {
                    Some(d) => d.clone(),
                    _ => Eval::Atom(Atom::Nil)
                }
            },
            _ => x.clone()
        }).collect::<Vec<Eval>>()
}

fn equals(args: &[Eval], env: &Env) -> EvalResult {
    if let Some(v) = resolve_symbols(args,env).first() {
        for i in args.iter().skip(1) {
            if v != i {
                return Ok(Eval::Atom(Atom::Boolean(false)))
            }
        }
        Ok(Eval::Atom(Atom::Boolean(true)))
    } else {
        Err(NotEnoughArguments)
    }
}

fn default_env() -> Env {
    Env{def_map: vec![HashMap::new()]}
}

fn first(args: &[Eval], env: &Env) -> EvalResult {
    if let Some(p) = resolve_symbols(args,env).first() {
        match p {
            &Eval::Atom(Atom::List(ref list)) => {
                if let Some(f) = list.front() {
                    Ok(Eval::Atom(f.clone()))
                } else {
                    Err(InvalidArguments)
                }
            },
            _ => Err(InvalidArguments)
        }
    } else {
        Err(NotEnoughArguments)
    }
}

use std::collections::VecDeque;

fn rest(args: &[Eval], env: &Env) -> EvalResult {
    if args.len() < 1 {
        return Err(NotEnoughArguments)
    }

    match &resolve_symbols(args,env)[0] {
        &Eval::Atom(Atom::List(ref list)) => {
            if list.len() > 1 {
                let mut out = VecDeque::with_capacity(list.len() - 1);

                for i in list.iter().skip(1) {
                    out.push_back(i.clone());
                }
                Ok(Eval::Atom(Atom::List(out)))
            } else {
                Ok(Eval::Atom(Atom::List(list.clone())))
            }
        },
        _ => Err(InvalidArguments)
    }
}

fn list(args: &[Eval], env: &Env) -> EvalResult {
    let mut out =  VecDeque::with_capacity(args.len());

    for i in resolve_symbols(args, env) {
        match i {
            Eval::Atom(a) => out.push_back(a.clone()),
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Eval::Atom(Atom::List(out)))
}

fn try_built_ins(sym: &str, args: &[Eval], env: &Env) -> Option<EvalResult> {
    match sym {
        "nil" => Some(Ok(Eval::Atom(Atom::Nil))),
        "+" => Some(add(args, env)),
        "-" => Some(sub(args, env)),
        "=" => Some(equals(args, env)),
        ">=" => Some(cmp(args, env, GreaterThanOrEqual)),
        ">" => Some(cmp(args, env, GreaterThan)),
        "<=" => Some(cmp(args, env, LessThanOrEqual)),
        "<" => Some(cmp(args, env, LessThan)),
        "*" => Some(mul(args, env)),
        "/" => Some(div(args,env)),
        "first" => Some(first(args,env)),
        "rest" => Some(rest(args,env)),
        "list" => Some(list(args,env)),
        _ => None
    }
}

fn define(args: &[Eval], env: &mut Env) -> EvalResult {
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

fn get(args: &[Eval], env: &Env) -> EvalResult {
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

fn eval_args(list: &[SyntaxNode], env: &mut Env) -> Result<Vec<Eval>, Error> {
    let mut args = vec![];

    for i in list.iter().skip(1) {
        let atom = try!(eval(i, env));
        args.push(atom);
    }
    Ok(args)
}

fn quote(v: &[Node]) -> Result<Eval, Error> {
    Ok(Eval::Node(v[1].clone()))
}

fn reduce_fn_params(l: &[SyntaxNode]) -> Result<Vec<Atom>,Error> {
    let mut v = vec![];

    for node in l {
        match node {
            &SyntaxNode::Node(Node::Atom(ref a)) => v.push(a.clone()),
            _ => {
                return Err(InvalidArguments)
            }
        }
    }
    Ok(v)
}

fn collect_body(l: &[SyntaxNode]) -> Vec<SyntaxNode> {
    l.iter().skip(2).map(|x| x.clone()).collect()
}

fn eval_node(atom: &Atom, list: &[SyntaxNode], env: &mut Env) -> EvalResult {
    match atom {
        &Atom::Symbol(ref s) =>  {
            match s.as_ref() {
                "def" => {
                    let args = try!(eval_args(&list, env));
                    return define(&args, env);
                },
                "fn" => {
                    let params = match list[1] {
                        SyntaxNode::Node(Node::List(ref l)) => try!(reduce_fn_params(l)),
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
                    panic!("not done");
                    //return quote(&list);
                },
                "get" => {
                    return get( &try!(eval_args(&list, env)), env);
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

fn eval(p: &SyntaxNode, env: &mut Env) -> Result<Eval, Error> {
    match p {
        &SyntaxNode::Node(ref node) => {
            match node {
                &Node::List(ref list) => {
                    if list.is_empty() {
                        return Err(InvalidArguments)
                    }
                    match list[0] {
                        SyntaxNode::Node(Node::Atom(ref atom)) => {
                            return eval_node(atom, list, env)
                        },
                        SyntaxNode::Node(Node::List(_)) => {
                            let r = try!(eval(&list[0], env));

                            match r {
                                Eval::Atom(a) => return eval_node(&a, list, env),
                                Eval::Proc(p) => return eval_procedure(&p, &try!(eval_args(&list, env)), env),
                                _ => {
                                    return Err(InvalidArguments)
                                }
                            }
                        },
                        _ => panic!("not ready")
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
        },
        &SyntaxNode::Quote(ref node) => {
            Ok(Eval::Node(node.clone()))
        }
        _ => panic!("argh")
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
                        Ok(r) => println!("{}", r),
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
    use super::Env;
    use super::EvalResult;

    fn teval(s: &str) -> Eval {
        eval(&tokenize(s).unwrap(), &mut default_env()).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> EvalResult {
        eval(&tokenize(s).unwrap(), env)
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

    fn tassert(v: &str) {
        assert_eq!(Eval::Atom(Atom::Boolean(true)), teval(v));
    }

    fn trefute(v: &str) {
        assert_eq!(Eval::Atom(Atom::Boolean(false)), teval(v));
    }

    #[test]
    fn comparisons() {
        tassert("(= 1 1 1 1)");
        trefute("(= 1 0 1 1)");
        tassert("(< 1 5 10)");
        trefute("(< 5 1 20)");
        tassert("(<= 1 1 1 5)");
        trefute("(<= 5 2 1 5)");
        tassert("(> 5 3 2 1)");
        trefute("(> 5 3 2 10)");
        tassert("(>= 5 5 5 3)");
        trefute("(>= 5 5 5 10)");
    }
    use super::number::Number;

    fn num(i: i64) -> Eval {
        Eval::Atom(Atom::Number(Number::Integer(i)))
    }

    #[test]
    fn adds() {
        assert_eq!(num(5), teval("(+ 2 3)"));
        assert_eq!(num(25), teval("(+ 5 5 5 5 5)"));
    }

    #[test]
    fn subs() {
        assert_eq!(num(0), teval("(- 5 5)"));
        assert_eq!(num(5), teval("(- 20 10 5)"));
    }

    #[test]
    fn muls() {
        assert_eq!(num(0), teval("(* 5 5 5 0)"));
        assert_eq!(num(5), teval("(* 1 5)"));
        assert_eq!(num(-5), teval("(* -1 5)"));
    }

    #[test]
    fn symbol_resolving() {
        let mut env = default_env();

        teval_env("(def foo 5)", &mut env).unwrap();

        assert_eq!(num(0), teval_env("(- foo 5)", &mut env).unwrap());
    }

    #[test]
    fn first_and_rest() {
        assert_eq!(num(0), teval("(first (list 0 1 2))"));
        assert_eq!(teval("(list 1 2)"), teval("(rest (list 0 1 2))"));
    }

    #[test]
    fn simple_func() {
        let mut env = default_env();
        let _ = eval(&tokenize("(def f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
        let res = eval(&tokenize("(f 2 3)").unwrap(), &mut env).unwrap();

        assert_eq!(num(5), res);
    }

    #[test]
    fn quoting() {
        //assert_eq!("(a (+ 1 2) c)", "'(a (+ 1 2) c)"); 
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
