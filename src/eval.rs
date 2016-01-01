use funcs::*;
use env::*;
use atom::*;
use errors::*;
use errors::Error::*;
use std::fmt::{Write};
use std::string::*;

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

fn eval_procedure(p: &Procedure, args: &[Atom], env: &mut Env) ->  AtomResult {
    env.apply(&p.params, args);

    let res = p.body.iter()
        .map(|node| eval(&node, env))
        .last().unwrap_or(Err(InvalidArguments));

    env.pop();

    res
}

fn eval_macro(p: &Procedure, args: &[Atom], env: &mut Env) -> AtomResult {
    env.apply(&p.params, args);

    let res = p.body.iter()
        .map(|node| eval(&node, env))
        .last().unwrap_or(Err(InvalidArguments))
        .and_then(|a| expand(&a, env, 0));

    env.pop();

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
    println!("quote is returning {}", print_list(list));
    list.first().map(|atom| atom.clone()).ok_or(InvalidArguments)
}

fn collect_body(l: &[Atom]) -> Vec<Atom> {
    l.iter().skip(2).map(|x| x.clone()).collect()
}

fn macroexpand(_: &[Atom], _: &Env) -> AtomResult {
    Err(NotImplemented)
}

fn do_form(list: &[Atom], env: &mut Env) -> AtomResult {
    list.iter()
        .map(|node| eval(node, env))
        .last().unwrap_or(Err(InvalidArguments))
}

fn make_proc(list: &[Atom]) -> AtomResult {
    if let Atom::List(ref p) = list[1] {
        Ok(Atom::Function(Function::Proc(Procedure{ params: p.clone(),
                                                    body: collect_body(list)})))
    } else {
        Err(InvalidArguments)
    }
}

fn macro_form(params: &Atom, list: Vec<Atom>) -> AtomResult {
    if let Atom::List(ref p) = *params {
        Ok(Atom::Function(Function::Macro(Procedure{ params: p.clone(),
                                                     body: list})))
    } else {
        Err(InvalidArguments)
    }
}

fn if_form(list: &[Atom], env: &mut Env) -> AtomResult {
    if list.len() < 2 {
        Err(InvalidArguments)
    } else {
        eval(&list[0], env)
            .and_then(|p| {
                let truthy = match p {
                    Atom::Boolean(b) => b,
                    Atom::Nil => false,
                    _ => true
                };
                if truthy {
                    eval(&list[1], env)
                } else if list.len() > 2 {
                    eval(&list[2], env)
                } else {
                    Ok(Atom::Boolean(false))
                }
            })
    }
}

fn eval_special_forms(f: Form, list: &[Atom], env: &mut Env) -> AtomResult {
    use atom::Form::*;
    match f {
        Def => {
            define(&list[1..], env)
        },
        Do => {
            do_form(&list[1..], env)
        },
        Macro => {
            unreachable!();
        }
        Fn => {
            make_proc(&list)
        },
        Quote => {
            quote(&list[1..])
        },
        If => {
            if_form(&list[1..], env)
        },
        MacroExpand => {
            macroexpand(&list[1..], env)
        },
        _ => Err(InvalidArguments)
    }
}

fn eval_node(atom: &Atom, list: &[Atom], env: &mut Env) -> AtomResult {
    println!("eval_node: {}", atom);
    match *atom {
        Atom::List(_) => {
            eval(atom, env)
        },
        Atom::Form(Form::QuasiQuote) => Ok(Atom::List(list.iter().map(|n| n.clone()).collect())),
        Atom::Symbol(_) =>  {
            let args: Vec<Atom> = try!(list.iter().map(|n| eval(n, env)).collect());
            eval_node(&args[0], &args, env)
        },
        Atom::Form(f) => eval_special_forms(f, list, env),
        Atom::Function(ref func) => {
            match *func {
                Function::Proc(ref p) => {
                    let args: Vec<Atom> = try!(list.iter().skip(1).map(|n| eval(&n, env)).collect());
                    eval_procedure(p, &args, env)
                }
                Function::Native(native) => {
                    let args: Vec<Atom> = try!(list.iter().skip(1).map(|n| eval(&n, env)).collect());
                    eval_native(native, &args, env)
                },
                Function::Macro(_) => unreachable!()
            }
        }
        _ => {
            println!("not a function: {:?}", atom);
            Err(Error::NotAFunction)
        }
    }
}

pub fn eval(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    println!("eval: {}", node);
    match *node {
        Atom::List(ref list) if !list.is_empty() => {
            eval(&list[0], env).and_then(|first| eval_node(&first, list, env))
        },
        Atom::Symbol(ref s) => {
            let a = env.get(s).unwrap_or(Atom::Nil);
            Ok(a)
        }
        _ => Ok(node.clone())
    }
}

fn expand_list(list: &[Atom], env: &mut Env) -> AtomResult {
    println!("expand_list: {}", print_list(list));
    if let Atom::Form(Form::Unquote) = list[0] {
        // ~x
        assert!(list.len() == 2);
        return expand(&list[1], env, 0)
    }

    let mut out = Vec::with_capacity(list.len());

    for i in list {
        if i.is_pair() {
            match *i {
                Atom::List(ref sublist) => {
                    match sublist[0] {
                        Atom::Form(Form::Splice) => {
                            println!("doing splice thangs");
                            assert!(sublist.len() == 2);
                            let first = try!(expand(&sublist[1], env, 0));
                            if let Atom::List(mut list) = first {
                                out.append(&mut list);
                                continue;
                            }
                        },
                        _ => ()
                    }
                }
                _ => ()
            }
        }

        let v = try!(expand_quasiquote(i, env));
        out.push(v);
    }

    Ok(Atom::List(out))
}

fn expand_quasiquote(atom: &Atom, env: &mut Env) -> AtomResult {
    println!("exp_quasi: {}", atom);
    if !atom.is_pair() {
        return Ok(Atom::List(vec![Atom::Form(Form::Quote), atom.clone()]))
    }

    if let Atom::List(ref list) = *atom {
        if let Atom::Form(Form::Splice) = list[0] {
            println!("can't splice here!");
            return Err(Error::Parser)
        }
        expand_list(list, env)
    } else {
        unreachable!("not here!")
    }
}

pub fn expand(node: &Atom, env: &mut Env, depth: i32) -> AtomResult {
    println!("[{}] expand: {}", depth, node);
    match *node {
        Atom::List(ref list) => {
            list.first().ok_or_else(|| {
                println!("in this expandy");
                Error::Parser})
                .and_then(|n| expand(n, env, depth + 1))
                .and_then(|n| expand_node(&n, list, env, depth + 1))
        },
        Atom::Symbol(ref sym) => {
            let a = env.get(sym).unwrap_or(node.clone());
            println!("expand resolving: {} -> {}", sym, a);
            println!("env contains {}", env);
            Ok(a)
        },
        _ => Ok(node.clone())
    }
}

fn expand_node(node: &Atom, list: &[Atom], env: &mut Env, depth: i32) -> AtomResult {
    println!("expand_node: {} - list: {}", node, print_list(list));
    match *node {
        Atom::Form(Form::Quote) => {
            Ok(Atom::List(list.iter().map(|n| n.clone()).collect()))
        }
        Atom::Form(Form::Unquote) => {
            Ok(node.clone())
        },
        Atom::Form(Form::Macro) => {
            let exps: Vec<Atom> = list.iter().skip(3).map(|n| n.clone()).collect();
            let m = try!(macro_form(&list[2], exps));

            if let Atom::Symbol(ref k) = list[1] {
                env.set(k.clone(), m);
                Ok(Atom::Nil)
            } else {
                Err(Error::InvalidArguments)
            }
        },
        Atom::Function(ref func) => {
            match *func {
                Function::Macro(ref m) => {
                    eval_macro(m, &list[1..], env).and_then(|a| expand(&a, env, depth + 1))
                },
                _ => {
                    Ok(Atom::List(try!(list.iter().map(|n| expand(n, env, depth + 1)).collect())))
                }
            }
        },
        Atom::Form(Form::QuasiQuote) => {
            expand_quasiquote(&list[1], env)
        }
        _ => {
            Ok(Atom::List(try!(list.iter().map(|n| expand(n, env, depth + 1)).collect())))
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
        let x = eval(&tokenize("(if (= 1 1) true false)").unwrap(), &mut Env::new()).unwrap();

        assert_eq!(Atom::from(true), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&tokenize("(if (= 1 2) true false)").unwrap(), &mut Env::new()).unwrap();

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
        let mut env = Env::new();
        tokenize(s).and_then(|n| expand(&n, &mut env, 0))
            .and_then(|n| eval(&n, &mut env)).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> AtomResult {
        tokenize(s).and_then(|n| {
            println!("<-- tokenize: {}", n);
            expand(&n, env, 0)
        }).and_then(|n| {
            println!("<-- expand: {}", n);
            eval(&n, env)
        })
    }

    #[test]
    fn symbol_resolving() {
        let mut env = Env::new();

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
        let mut env = Env::new();
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
        assert_eq!(teval("4"), teval("(do (+ 1 2) (+ 2 2))"));
    }

    #[test]
    fn splice_macro() {
        let mut env = Env::new();

        teval_env("(defmacro foo (& a) `(list ~@a))", &mut env).unwrap();

        let r = teval_env("(foo 1 2)", &mut env).unwrap();

        assert_eq!(teval("'(1 2)"), r);
        //assert_eq!(teval("'(1 2)"), teval_env("(foo '(1 2))", &mut env).unwrap());
    }

    #[test]
    fn back_tick_simple_list() {
        assert_eq!(teval("'(1 2)"), teval("`(1 2)"));
    }
    // #[test]
    //  fn simple_macro() {
    //      let mut env = Env::new();
    //      teval_env("(defmacro m () '(+ 3 5))", &mut env).unwrap();
    //      assert_eq!(teval("'(+ 3 5)"), teval_env("(macroexpand '(m))", &mut env).unwrap());

    //      teval_env("(def mm (macro () (+ 3 5)))", &mut env).unwrap();
    //      assert_eq!(teval("8"), teval_env("(macroexpand '(mm))", &mut env).unwrap());

    //      assert_eq!(teval("8"), teval_env("(m)", &mut env).unwrap());
    //      assert_eq!(teval("8"), teval_env("(mm)", &mut env).unwrap());
    //  }

    #[test]
    fn more_complex_macro() {
        let mut env = Env::new();
        teval_env("(defmacro unless (p a b) `(if (not ~p) ~a ~b))", &mut env).unwrap();
        assert_eq!(teval("3"), teval_env("(unless false 3 5)", &mut env).unwrap());
    }

    #[test]
    fn recursive_fibonacci() {
        let mut env = Env::new();
        teval_env("(def fibonacci (fn (n) (if (<= n 2) 1 (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))", &mut env).unwrap();

        assert_eq!(teval("1"), teval_env("(fibonacci 0)", &mut env).unwrap());
        assert_eq!(teval("2"), teval_env("(fibonacci 3)", &mut env).unwrap());
        assert_eq!(teval("55"), teval_env("(fibonacci 10)", &mut env).unwrap());
    }
}
