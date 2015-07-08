use funcs::*;
use env::*;
use atom::*;
use errors::*;
use errors::Error::*;

fn eval_procedure(p: &Procedure, args: &[Atom], env: &mut Env) ->  AtomResult {
    try!(env.apply(&p.params, &args[1..]));

    let res = p.body.iter()
        .map(|node| eval(&node, env))
        .last().unwrap_or(Err(InvalidArguments));

    env.pop();

    res
}

fn eval_macro(p: &Procedure, args: &[Atom], env: &Env) -> AtomResult {
    Err(NotImplemented)
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

fn quote(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    Err(NotImplemented)
    // match *node {
    //     Atom::List(ref list) => {
    //         let mut out = VecDeque::with_capacity(list.len());
    //         for i in list {
    //             let n = try!(eval(i, env));

    //             match n {
    //                 Atom::Atom(ref a) => {
    //                     out.push_back(a.clone());
    //                 }
    //                 _ => {
    //                     return Err(Error::InvalidArguments)
    //                 }
    //             }
    //         }
    //         return Ok(Atom::Atom(Atom::List(out)))
    //     },
    //     _ => Ok(node.clone())
    // }
}

fn collect_body(l: &[Atom]) -> Vec<Atom> {
    l.iter().skip(2).map(|x| x.clone()).collect()
}

fn macroexpand(list: &[Atom], env: &Env) -> AtomResult {
    Err(NotImplemented)
}

fn do_form(list: &[Atom], env: &mut Env) -> AtomResult {
    list.iter()
        .map(|node| eval(&node, env))
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
            Err(NotImplemented)
        }
        Fn => {
            make_proc(&list)
        },
        Quote => {
            //if list.len() > 2 {
            //     return Err(InvalidArguments)
            // }
            // match list[1] {
            //     Atom::Atom(ref node) => {
            //         return quote(node, env)
            //     }
            //     _ => {
            //         return Err(InvalidArguments)
            //     }
            // }
            Err(NotImplemented)
        },
        If => {
            if list.len() < 2 {
                 Err(InvalidArguments)
            } else {
                eval(&list[1], env)
                    .and_then(|p| {
                        let truthy = match p {
                             Atom::Boolean(b) => b,
                            Atom::Nil => false,
                            _ => true
                        };
                        if truthy {
                            eval(&list[2], env)
                        } else if list.len() > 2 {
                            eval(&list[3], env)
                        } else {
                            Ok(Atom::Boolean(false))
                        }
                    }
                         )
            }
        },
    }
}

fn eval_node(atom: &Atom, list: &[Atom], env: &mut Env) -> AtomResult {
    match *atom {
        Atom::Symbol(_) =>  {
            let args: Vec<Atom> = try!(list.iter().map(|n| eval(n, env)).collect());
            eval_node(&args[0], &args, env)
        },
        Atom::Form(f) => eval_special_forms(f, list, env),
        Atom::Function(ref func) => {
            match *func {
                Function::Proc(ref p) => {
                    eval_procedure(p, list, env)
                }
                Function::Native(native) => {
                    eval_native(native, &list[1..], env)
                }
            }
        }
        _ => {
            println!("{:?}", atom);
            Err(Error::NotAFunction)
        }
    }
}

pub fn eval(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    match *node {
        Atom::List(ref list) if !list.is_empty() => {
            eval(&list[0], env).and_then(|first| eval_node(&first, list, env))
        },
        Atom::Symbol(ref s) => {
            match env.get(s) {
                Some(e) => Ok(e),
                _ => Ok(Atom::Nil)
            }
        }
        _ => Ok(node.clone())
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
        eval(&tokenize(s).unwrap(), &mut Env::new()).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> AtomResult {
        eval(&tokenize(s).unwrap(), env)
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

    // #[test]
    // fn first_and_rest() {
    //     assert_eq!(num(0), teval("(first (list 0 1 2))"));
    //     assert_eq!(teval("(list 1 2)"), teval("(rest (list 0 1 2))"));
    // }

    #[test]
    fn simple_func() {
        let mut env = Env::new();
        let _ = eval(&tokenize("(def f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
        let res = eval(&tokenize("(f 2 3)").unwrap(), &mut env).unwrap();

        assert_eq!(num(5), res);
    }

    // #[test]
    // fn quote_func() {
    //     assert_eq!(teval("'(1 2)"), teval("(quote (1 2))"));
    //     assert_eq!(teval("'a"), teval("(quote a)"));
    // }

    #[test]
    fn do_func() {
        assert_eq!(teval("4"), teval("(do (+ 1 2) (+ 2 2))"));
    }

    // #[test]
    // fn simple_macro() {
    //     let mut env = Env::new();

    //     teval_env("(def 'm (macro () (+ 3 5))", &mut env).unwrap();
    //     assert_eq!(teval("'(+ 3 5)"), teval_env("(m)", &mut env).unwrap());
    // }
}
