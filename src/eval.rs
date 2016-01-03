use funcs::*;
use env::*;
use atom::*;
use errors::*;
use errors::Error::*;
use std::fmt::{Write};
use std::string::*;
use smallvec::SmallVec;

#[allow(dead_code)]
fn print_list(list: &[Atom]) -> String {
    let mut s = String::new();
    write!(&mut s,"[").unwrap();

    if let Some(first) = list.first() {
        write!(&mut s, "{}", first).unwrap();
        for i in list.iter().skip(1) {
            write!(&mut s, " {}", i).unwrap();
        }
    }
    write!(&mut s, "]").unwrap();
    s
}

fn eval_macro(p: &Procedure, args: &[Atom], env: &mut Env) -> AtomResult {
    let mut e = env.bind(&p.params, args);

    let res = p.body.iter()
        .map(|node| eval(&node, &mut e))
        .last().unwrap_or(Err(InvalidArguments));

    res
}


fn define(args: &[Atom], env: &mut Env) -> AtomResult {
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }

    if let Atom::Symbol(ref key) = args[0] {
        let value = try!(eval(&args[1], env));
        env.set(key.clone(), value);
        Ok(Atom::Nil)
    } else {
        Err(Error::InvalidArguments)
    }
}

fn quote(list: &[Atom]) -> AtomResult {
    list.first().map(|atom| atom.clone()).ok_or(InvalidArguments)
}

fn macroexpand(_: &[Atom], _: &Env) -> AtomResult {
    Err(NotImplemented)
}

fn make_proc(list: &[Atom], env: &Env) -> AtomResult {
    if let Atom::List(ref p) = list[1] {
        let mut body = Vec::with_capacity(list.len());
        body.push(Atom::Form(Form::Do));

        for i in list.iter().skip(2) {
            body.push(i.clone());
        }

        Ok(Atom::Function(Function::Proc(Procedure{ params: p.clone(),
                                                    body: body,
                                                    closures: Env::new(Some(env.clone()))})))
    } else {
        Err(InvalidArguments)
    }
}

fn macro_form(args: &[Atom], env: &mut Env) -> AtomResult {
    if args.len() < 3 {
        return Err(InvalidArguments)
    }

    let name = match args[0] {
        Atom::Symbol(name) => name,
        _ => return Err(InvalidArguments)
    };
    let params = match args[1] {
        Atom::List(ref list) => {
            list.iter().map(|n| n.clone()).collect()
        },
        _ => return Err(InvalidArguments)
    };

    let p = Procedure {
        body: args[2..].iter().map(|n| n.clone()).collect(),
        params: params,
        closures: Env::new(Some(env.clone()))
    };

    env.set(name, Atom::Function(Function::Macro(p)));

    Ok(Atom::Symbol(name))
}

fn expand_quasiquote(node: &Atom, env: &mut Env) -> AtomResult {
    if !node.is_pair() {
        return Ok(Atom::List(vec![Atom::Form(Form::Quote), node.clone()]))
    }

    match *node {
        Atom::List(ref list) => {
            match list[0] {
                Atom::Form(Form::Unquote) => {
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
                                return Ok(Atom::List(vec![append, sublist[1].clone(),
                                                          Atom::List(rest)]))
                            },
                            _ => ()
                        }
                    },
                    _ => ()
                }
            }

            let rest = list[1..].iter().map(|n| n.clone()).collect();
            return Ok(Atom::List(vec![Atom::Function(Function::Native(Native::Cons)),
                                      try!(expand_quasiquote(&list[0], env)),
                                      try!(expand_quasiquote(&Atom::List(rest), env))]))
        },
        _ => unreachable!()
    }
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
            macroexpand(&list[1..], env)
        },
        _ => Err(InvalidArguments)
    }
}

pub fn eval_node(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    match *node {
        Atom::Symbol(sym) => {
            match env.get(sym.as_ref()) {
                Some(v) => Ok(v),
                None => Ok(Atom::Nil)
            }
        },
        Atom::List(ref list) => {
            Ok(Atom::List(try!(list.iter().map(|n| eval(&n, env)).collect())))
        },
        _ => Ok(node.clone())
    }
}

fn macro_expand(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    match *node {
        Atom::List(ref list) => {
            match list[0] {
                Atom::Symbol(ref sym) => {
                    if let Some(Atom::Function(Function::Macro(ref m))) = env.get(sym.as_ref()) {
                        return macro_expand(&try!(eval_macro(m, &list[1..], env)), env)
                    }
                },
                Atom::Function(Function::Macro(ref m)) => {
                    return macro_expand(&try!(eval_macro(m, &list[1..], env)), env)
                }
                _ => ()
            }
        },
        _ => ()
    }

    Ok(node.clone())
}

pub fn eval(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    let mut cur_node = node.clone(); // FIXME: shitty clone
    let mut cur_env = env;

    loop {
        match cur_node {
            Atom::List(ref list) => {
                if list.is_empty() {
                    return Ok(Atom::Nil)
                }
                ()
            },
            _ => return eval_node(&cur_node, cur_env)
        }

        let expanded_node = try!(macro_expand(&cur_node, cur_env));
        let mut list = match expanded_node {
            Atom::List(l) => l,
            _ => return Ok(expanded_node)
        };

        match list[0] {
            Atom::Symbol(_) | Atom::List(_) => {
                list[0] = try!(eval(&list[0], cur_env));
            },
            _ => ()
        }

        match list[0] {
            Atom::Form(f) => {
                match f {
                    Form::Do => {
                        if list.len() == 1 {
                            return Ok(Atom::Nil)
                        }
                        if list.len() > 2 {
                            for i in 1..list.len()-1 {
                                try!(eval(&list[i], cur_env));
                            }
                        }
                        cur_node = list[list.len()-1].clone();
                    },
                    Form::QuasiQuote => {
                        cur_node = try!(expand_quasiquote(&list[1], cur_env));
                    },
                    Form::If => {
                        let condition = match try!(eval(&list[1], cur_env)) {
                            Atom::Boolean(b) => b,
                            Atom::Nil => false,
                            _ => true
                        };

                        if condition {
                            cur_node = list[2].clone();
                        } else if list.len() > 2 {
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
                let args: SmallVec<[Atom;4]> = try!(list.iter().skip(1).map(|n| eval(&n, cur_env)).collect());
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
        let x = eval(&tokenize("(if (= 1 1) true false)").unwrap(), &mut Env::new(None)).unwrap();

        assert_eq!(Atom::from(true), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&tokenize("(if (= 1 2) true false)").unwrap(), &mut Env::new(None)).unwrap();

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
            .and_then(|n| eval(&n, &mut env)).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> AtomResult {
        tokenize(s).and_then(|n| {
            eval(&n, env)
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
        let _ = eval(&tokenize("(def f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
        let res = eval(&tokenize("(f 2 3)").unwrap(), &mut env).unwrap();

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
}
