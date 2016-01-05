use funcs::*;
use env::*;
use atom::*;
use errors::*;
use errors::Error::*;
use std::fmt::{Write};
use std::string::*;
use smallvec::SmallVec;
use std::rc::Rc;

#[allow(dead_code)]
fn print_list(list: &[Atom]) -> String {
    let mut s = String::new();
    write!(&mut s,"(").unwrap();

    if let Some(first) = list.first() {
        write!(&mut s, "{}", first).unwrap();
        for i in list.iter().skip(1) {
            write!(&mut s, " {}", i).unwrap();
        }
    }
    write!(&mut s, ")").unwrap();
    s
}

fn eval_macro(p: &Procedure, args: &[Atom], env: &mut Env) -> AtomResult {
    let mut e = env.bind(&p.params, args);

    eval(Atom::List(p.body.clone()), &mut e)
}

fn define(args: &[Atom], env: &mut Env) -> AtomResult {
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }

    let key = try!(args[0].as_symbol());
    let value = try!(eval(args[1].clone(), env));
    env.define(*key, value)
}

fn quote(list: &[Atom]) -> AtomResult {
    try!(expect_arg_length(list, 1));
    list.first().map(|atom| atom.clone()).ok_or(InvalidArguments)
}

fn make_proc(list: &[Atom], env: &Env) -> AtomResult {
    let p = try!(list[1].as_list());
    let mut body = Vec::with_capacity(list.len());
    body.push(Atom::Form(Form::Do));

    for i in list.iter().skip(2) {
        body.push(i.clone());
    }
    Ok(Atom::Function(Function::Proc(Procedure{ params: p.clone(),
                                                body: Rc::new(body),
                                                closures: Env::new(Some(env.clone()))})))
}

fn macro_form(args: &[Atom], env: &mut Env) -> AtomResult {
    if args.len() < 3 {
        return Err(InvalidArguments)
    }

    let name = try!(args[0].as_symbol());

    let params = try!(args[1].as_list()).clone();

    let mut body = Vec::with_capacity(args.len()-1);
    body.push(Atom::Form(Form::Do));

    for node in &args[2..] {
        body.push(node.clone());
    }

    let p = Procedure {
        body: Rc::new(body),
        params: params,
        closures: Env::new(Some(env.clone()))
    };

    env.define(*name, Atom::Function(Function::Macro(p)))
}

fn expand_quasiquote(node: &Atom, env: &mut Env) -> AtomResult {
    if !node.is_pair() {
        return Ok(Atom::list(vec![Atom::Form(Form::Quote), node.clone()]))
    }

    let list = try!(node.as_list());
    match list[0] {
        Atom::Form(Form::Unquote) => {
            try!(expect_arg_length(list, 2));
            return Ok(list[1].clone())
        },
        _ => ()
    }

    if list[0].is_pair() {
        match list[0] {
            Atom::List(ref sublist) => {
                match sublist[0] {
                    Atom::Form(Form::Splice) => {
                        let append = Atom::Function(Function::Native(Native::Append));
                        let rest = list[1..].iter().map(|n| n.clone()).collect();
                        return Ok(Atom::list(vec![append, sublist[1].clone(),
                                                  Atom::list(rest)]))
                    },
                    _ => ()
                }
            },
            _ => ()
        }
    }

    let rest = list[1..].iter().map(|n| n.clone()).collect();
    Ok(Atom::list(vec![Atom::Function(Function::Native(Native::Cons)),
                       try!(expand_quasiquote(&list[0], env)),
                       try!(expand_quasiquote(&Atom::list(rest), env))]))
}

fn eval_special_forms(f: Form, list: &[Atom], env: &mut Env) -> AtomResult {
    use atom::Form::*;
    match f {
        Def => {
            define(&list[1..], env)
        },
        Macro => {
            macro_form(&list[1..], env)
        }
        Fn => {
            make_proc(&list, env)
        },
        Quote => {
            quote(&list[1..])
        },
        MacroExpand => {
            macro_expand(list[1].clone(), env)
        },
        _ => Err(InvalidArguments)
    }
}

pub fn eval_node(node: Atom, env: &mut Env) -> Result<Atom, Error> {
    match node {
        Atom::Symbol(sym) => {
            Ok(env.get(sym.as_ref()).unwrap_or(Atom::Nil))
        },
        Atom::List(ref list) => {
            Ok(Atom::list(try!(list.iter().map(|n| eval(n.clone(), env)).collect())))
        },
        _ => Ok(node)
    }
}

fn macro_expand(node: Atom, env: &mut Env) -> Result<Atom, Error> {
    match node {
        Atom::List(ref list) if !list.is_empty() => {
            match list[0] {
                Atom::Symbol(ref sym) => {
                    if let Some(Atom::Function(Function::Macro(ref m))) = env.get(sym.as_ref()) {
                        return macro_expand(try!(eval_macro(m, &list[1..], env)), env)
                    }
                },
                Atom::Form(Form::Fn) => {
                    if list.len() < 3 {
                        return Err(Error::NotEnoughArguments)
                    }

                    let mut out = Vec::with_capacity(list.len());
                    out.push(list[0].clone());
                    out.push(list[1].clone());
                    for i in &list[2..] {
                        out.push(try!(macro_expand(i.clone(), env)));
                    }
                    return Ok(Atom::list(out))
                }
                _ => ()
            }
            return Ok(Atom::list(try!(list.iter().map(|n| macro_expand(n.clone(), env)).collect())))
        },
        _ => ()
    }

    Ok(node)
}

pub fn expect_arg_length(args: &[Atom], len: usize) -> Result<(), Error> {
    if args.len() == len {
        Ok(())
    } else {
        Err(Error::NotEnoughArguments)
    }
}

#[allow(dead_code)]
pub fn trace(node: Atom, msg: &str) -> Atom {
    println!("{}: {}", msg, node);
    node
}

pub fn eval(node: Atom, env: &mut Env) -> Result<Atom, Error> {
    let mut cur_node = node;
    let mut cur_env = env;

    loop {
        match cur_node {
            Atom::List(ref list) => {
                if list.is_empty() {
                    return Ok(Atom::Nil)
                }
                ()
            },
            _ => return eval_node(cur_node, cur_env)
        }

        let expanded_node = try!(macro_expand(cur_node, cur_env));
        let mut list2 = match expanded_node {
            Atom::List(l) => l,
            _ => return Ok(expanded_node)
        };

        let list = Rc::get_mut(&mut list2).unwrap(); // FIXME: hack either lists should be immutable or not

        match list[0] {
            Atom::Symbol(_) | Atom::List(_) => {
                list[0] = try!(eval(list[0].clone(), cur_env));
            }
            _ => ()
        }

        //println!("evaling: {}", print_list(&list));

        match list[0] {
            Atom::Form(f) => {
                match f {
                    Form::Let => {
                        try!(expect_arg_length(list, 3));
                        let mut env = Env::new(Some(cur_env.clone()));

                        let bind_list = try!(list[1].as_list());
                        for binding in bind_list.iter() {
                            let bind_exp = try!(binding.as_list());
                            try!(expect_arg_length(bind_exp, 2));
                            let symbol = try!(bind_exp[0].as_symbol());
                            let value = try!(eval(bind_exp[1].clone(), &mut env));
                            try!(env.define(*symbol, value));
                        }
                        cur_node = try!(eval(list[2].clone(), &mut env));
                    }
                    Form::Set => {
                        try!(expect_arg_length(list, 3));

                        let symbol = try!(list[1].as_symbol());
                        let value = try!(eval(list[2].clone(), cur_env));

                        return cur_env.set(*symbol, value)
                    },
                    Form::Do => {
                        if list.len() == 1 {
                            return Ok(Atom::Nil)
                        }
                        if list.len() > 2 {
                            for i in 1..list.len()-1 {
                                try!(eval(list[i].clone(), cur_env));
                            }
                        }
                        cur_node = list[list.len()-1].clone();
                    },
                    Form::QuasiQuote => {
                        cur_node = try!(expand_quasiquote(&list[1], cur_env));
                    },
                    Form::If => {
                        if list.len() < 2 {
                            return Err(Error::NotEnoughArguments)
                        }

                        let condition = try!(eval(list[1].clone(), cur_env)).as_bool();

                        if condition {
                            cur_node = list[2].clone();
                        } else if list.len() > 3 {
                            cur_node = list[3].clone();
                        } else {
                            return Ok(Atom::Boolean(false))
                        }
                    },
                    _ => {
                        return eval_special_forms(f, &list, cur_env)
                    }
                }
            },
            Atom::Function(ref func) => {
                let args: SmallVec<[Atom;4]> = try!(list.iter().skip(1).map(|n| eval(n.clone(), cur_env)).collect());
                // println!("args are: {}", print_list(&args));
                match *func {
                    Function::Proc(ref p) => {
                        *cur_env = Env::new(Some(p.closures.clone())).bind(&p.params, &args);
                        cur_node = Atom::List(p.body.clone());
                    },
                    Function::Native(native) => {
                        return eval_native(native, &args, cur_env)
                    },
                    _ => {
                        panic!("no macros here!")
                    }
                }
            },
            _ => {
                println!("{} isnt' a function", list[0]);
                return Err(Error::NotAFunction)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use atom::*;
    use env::Env;

    #[test]
    fn if_special_form() {
        let x = eval(tokenize("(if (= 1 1) true false)").unwrap(), &mut Env::new(None)).unwrap();

        assert_eq!(Atom::from(true), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(tokenize("(if (= 1 2) true false)").unwrap(), &mut Env::new(None)).unwrap();

        assert_eq!(Atom::Boolean(false), x);
    }

    #[test]
    fn if_no_else() {
        let x = teval("(if (= 1 1) true)");

        assert_eq!(Atom::Boolean(true), x);
    }

    fn tassert(v: &str) {
        assert_eq!(Atom::Boolean(true), teval(v));
    }

    fn trefute(v: &str) {
        assert_eq!(Atom::Boolean(false), teval(v));
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
    use number::Number;

    fn num(i: i64) -> Atom {
        Atom::Number(Number::Integer(i))
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

    fn teval(s: &str) -> Atom {
        let mut env = Env::new(None);
        tokenize(s)
            .and_then(|n| eval(n, &mut env)).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> AtomResult {
        tokenize(s).and_then(|n| {
            eval(n, env)
        })
    }

    #[test]
    fn symbol_resolving() {
        let mut env = Env::new(None);

        teval_env("(def foo 5)", &mut env).unwrap();

        assert_eq!(num(0), teval_env("(- foo 5)", &mut env).unwrap());
    }

    #[test]
    fn no_def_func() {
        assert_eq!(teval("3"), teval("((fn (a b) (+ a b)) 1 2)"));
    }

    #[test]
    fn first_and_rest() {
        assert_eq!(num(0), teval("(first '(0 1 2))"));
        assert_eq!(teval("'(1 2)"), teval("(rest '(0 1 2))"));
    }

    #[test]
    fn simple_func() {
        let mut env = Env::new(None);
        let _ = eval(tokenize("(def f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
        let res = eval(tokenize("(f 2 3)").unwrap(), &mut env).unwrap();

        assert_eq!(num(5), res);
    }

    #[test]
    fn quote_func() {
        assert_eq!(teval("'(1 2)"), teval("(quote (1 2))"));
        assert_eq!(teval("'a"), teval("(quote a)"));
    }

    #[test]
    fn do_func() {
        assert_eq!(teval("()"), teval("(do)"));
        assert_eq!(teval("4"), teval("(do (+ 1 2) (+ 2 2))"));
    }

    #[test]
    fn splice_macro() {
        let mut env = Env::new(None);

        teval_env("(defmacro foo (& a) `(list ~@a))", &mut env).unwrap();

        let r = teval_env("(foo 1 2)", &mut env).unwrap();

        assert_eq!(teval("'(1 2)"), r);
        //assert_eq!(teval("'(1 2)"), teval_env("(foo '(1 2))", &mut env).unwrap());
    }

    #[test]
    fn back_tick_simple_list() {
        assert_eq!(teval("'(1 2)"), teval("`(1 2)"));
    }

    #[test]
    fn back_tick_subst() {
        assert_eq!(teval("'(1 3)"), teval("`(1 ~@(+ 1 2))"));
    }


    #[test]
    fn more_complex_macro() {
        let mut env = Env::new(None);
        teval_env("(defmacro unless (p a b) `(if (not ~p) ~a ~b))", &mut env).unwrap();
        assert_eq!(teval("3"), teval_env("(unless false 3 5)", &mut env).unwrap());
    }

    #[test]
    fn recursive_fibonacci() {
        // need a tail call version 
        let t = include_str!("../test/recursion.lisp");

        assert_eq!(teval(t), teval("9227465"));
    }

    #[test]
    #[should_panic]
    fn numbers_arent_functions() {
        teval("(1 1)");
    }

    #[test]
    fn tailcall_function() {
        let mut env = Env::new(None);

        teval_env("(def sum2 (fn (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))", &mut env).unwrap();

        assert_eq!(teval_env("(sum2 10 0)", &mut env).unwrap(), teval("55"));
        assert_eq!(teval_env("(sum2 10000 0)", &mut env).unwrap(), teval("50005000"));


        teval_env("(def foo (fn (n) (if (= n 0) 0 (bar (- n 1)))))", &mut env).unwrap();
        teval_env("(def bar (fn (n) (if (= n 0) 0 (foo (- n 1)))))", &mut env).unwrap();

        assert_eq!(teval_env("(foo 10000)", &mut env).unwrap(), teval("0"));
        teval_env("(def sum-to (fn (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))", &mut env).unwrap();
        assert_eq!(teval_env("(sum-to 10)", &mut env).unwrap(), teval("55"));
    }

    use base_lib;

    #[test]
    fn let_tests() {
        let mut e = Env::new(None);
        base_lib::init(&mut e).unwrap();
        teval_env(include_str!("../test/let.lisp"), &mut e).unwrap();
    }

    #[test]
    fn non_tco() {
        let mut e = Env::new(None);
        teval_env("(def sum-to (fn (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))", &mut e).unwrap();
        assert_eq!(teval("55"), teval_env("(sum-to 10)", &mut e).unwrap());
    }

    use test::Bencher;

    #[bench]
    fn recursion_bench(b: &mut Bencher) {
        let mut env = Env::new(None);
        teval_env("(def sum2 (fn (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))", &mut env).unwrap();

        b.iter(|| {
            teval_env("(sum2 100 0)", &mut env).unwrap()
        })
    }
}
