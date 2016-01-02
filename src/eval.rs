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
        .last().unwrap_or(Err(InvalidArguments));
        
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

    let body = args[2..].iter().map(|n| n.clone()).collect();

    env.set(name, Atom::Function(Function::Macro(Procedure{ params: params, body: body})));

    Ok(Atom::Symbol(name))
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
            macro_form(&list[1..], env)
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
    println!("eval: {}", node);

    let pre_list = match *node {
        Atom::List(ref list) => list,
        _ => return eval_node(node, env)
    };

    if pre_list.is_empty() {
        return Ok(Atom::Nil)
    }

    let expanded_node = try!(macro_expand(node, env));
    let mut list = match expanded_node {
        Atom::List(l) => l,
        _ => return Ok(expanded_node)
    };
    
    match list[0] {
        Atom::Symbol(_) | Atom::List(_) => {
            list[0] = try!(eval(&list[0], env));
            return eval(&Atom::List(list), env)
        },
        _ => ()
    }

    match list[0] {
        Atom::Form(f) => {
            return eval_special_forms(f, &list, env)
        },
        Atom::Function(ref func) => {
            let args: Vec<Atom> = try!(list.iter().skip(1).map(|n| eval(&n, env)).collect());
            match *func {
                Function::Proc(ref p) => {
                    return eval_procedure(p, &args, env)
                },
                Function::Native(native) => {
                    return eval_native(native, &args, env)
                },
                _ => {
                    panic!("no macros here!")
                }
            }
        },
        _ => return Err(Error::NotAFunction)
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

    #[test]
    fn back_tick_subst() {
        assert_eq!(teval("'(1 3)"), teval("`(1 ~@(+ 1 2))"));
    }


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

    #[test]
    #[should_panic]
    fn numbers_arent_functions() {
        teval("(1 1)");
    }
}
