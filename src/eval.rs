use funcs::*;
use env::*;
use parser::*;
use atom::*;
use errors::*;
use errors::Error::*;
use std::collections::VecDeque;

fn eval_procedure(p: &Procedure, args: &[Atom], env: &mut Env) ->  AtomResult {
    try!(env.apply(&p.params, args));

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
        env.set(key.clone(), args[1].clone());
        Ok(Atom::Nil)
    } else {
        Err(Error::InvalidArguments)
    }
}
fn get(args: &[Atom], env: &Env) -> AtomResult {
    if args.len() != 1 {
        return Err(Error::InvalidArguments)
    }

    if let Atom::Symbol(ref s) = args[0] {
        if let Some(a) = env.get(s) {
            Ok(a.clone())
        } else {
            Ok(Atom::Nil)
        }
    } else {
        Err(Error::InvalidArguments)
    }
}

fn eval_args(list: &[Atom], env: &mut Env) -> Result<Vec<Atom>, Error> {
    list.iter().skip(1).map(|i| eval(i, env)).collect()
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

fn reduce_fn_params(l: &[Atom]) -> Result<Vec<Atom>,Error> {
    Ok(l.iter().map(|node|
                 node.clone()
                 ).collect())
}

fn collect_body(l: &[Atom]) -> Vec<Atom> {
    l.iter().skip(2).map(|x| x.clone()).collect()
}

fn macroexpand(list: &[Atom], env: &Env) -> AtomResult {
    Err(NotImplemented)
}

fn do_form(list: &[Atom], env: &mut Env) -> AtomResult {
    list.iter()
        .skip(1)
        .map(|node| eval(&node, env))
        .last().unwrap_or(Err(InvalidArguments))
}

fn make_proc(list: &[Atom]) -> Result<Procedure, Error> {
    Err(NotImplemented)
    // let params = match list[1] {
    //     Atom::Atom(Atom::List(ref l)) => try!(reduce_fn_params(l)),
    //     _ => {
    //         return Err(InvalidArguments)
    //     }
    // };
    // Ok(Procedure{ params: params,
    //            body: collect_body(list)
    // })
}

fn eval_node(atom: &Atom, list: &[Atom], env: &mut Env) -> AtomResult {
    match *atom {
        Atom::Symbol(ref s) =>  {
            match s.as_ref() {
                "def" => {
                    let args = try!(eval_args(&list, env));
                    return define(&args, env);
                },
                "do" => {
                    return do_form(list, env)
                },
                "macro" => {
                    return Err(NotImplemented)
                    // if env.depth() > 1 {
                    //     return Err(InvalidArguments)
                    // }
                    // return make_proc(&list)
                    //     .map(|p| Atom::Macro(p))
                }
                "macroexpand" => {
                    return Err(NotImplemented)
                },
                "fn" => {
                    return Err(NotImplemented)
                    // return make_proc(&list)
                    //     .map(|p| Atom::Proc(p))
                },
                "quote" => {
                    // if list.len() > 2 {
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
                    return Err(NotImplemented)
                },
                "get" => {
                    return get( &try!(eval_args(&list, env)), env);
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

            if let Some(r) = try_built_ins(&s, &args, env) {
                return r
            }

            // if let Some(&Atom::Proc(ref p)) = env.get(&s) {
            //     let mut renv = env.clone();
            //     let r = eval_procedure(p, &args, &mut renv);
            //     return r
            // }

            Ok(Atom::Nil)
        },
        _ => {
            Err(Error::NotAFunction)
        }
    }
}

pub fn eval(node: &Atom, env: &mut Env) -> Result<Atom, Error> {
    match *node {
        Atom::List(ref list) => {
            if list.is_empty() {
                return Err(InvalidArguments)
            } else {
                panic!("blag")
            }
            // match list[0] {
            //     Atom(ref atom) => {
            //         return eval_node(atom, list, env)
            //     },
            //     Atom::Atom(Atom::List(_)) => {
            //         let r = try!(eval(&list[0], env));
            //         match r {
            //             Atom::Atom(a) => return eval_node(&a, list, env),
            //             Atom::Proc(p) => return eval_procedure(&p, &try!(eval_args(&list, env)), env),
            //             _ => {
            //                 return Err(InvalidArguments)
            //             }
            //         }
            //     },
            //     _ => panic!("not ready")
            // }
        },
        Atom::Symbol(ref s) => {
            match env.get(s) {
                Some(e) => return Ok(e.clone()),
                _ => return Ok(Atom::Nil)
            }
        }
        _ => Ok(node.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use atom::Atom;
    use env::Env;
    use expr::*;

    #[test]
    fn if_special_form() {
        let x = eval(&tokenize("(if (= 1 1) true false)").unwrap(), &mut Env::new()).unwrap();

        assert_eq!(Atom::Atom(Atom::Boolean(true)), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&tokenize("(if (= 1 2) true false)").unwrap(), &mut Env::new()).unwrap();

        assert_eq!(Expr::Atom(Atom::Boolean(false)), x);
    }

    #[test]
    fn if_no_else() {
        let x = teval("(if (= 1 1) true)");

        assert_eq!(Expr::Atom(Atom::Boolean(true)), x);
    }

    fn tassert(v: &str) {
        assert_eq!(Expr::Atom(Atom::Boolean(true)), teval(v));
    }

    fn trefute(v: &str) {
        assert_eq!(Expr::Atom(Atom::Boolean(false)), teval(v));
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

    fn num(i: i64) -> Expr {
        Expr::Atom(Atom::Number(Number::Integer(i)))
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

    fn teval(s: &str) -> Expr {
        eval(&tokenize(s).unwrap(), &mut Env::new()).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> ExprResult {
        eval(&tokenize(s).unwrap(), env)
    }

    #[test]
    fn symbol_resolving() {
        let mut env = Env::new();

        teval_env("(def 'foo 5)", &mut env).unwrap();

        assert_eq!(num(0), teval_env("(- foo 5)", &mut env).unwrap());
    }

    #[test]
    fn first_and_rest() {
        assert_eq!(num(0), teval("(first (list 0 1 2))"));
        assert_eq!(teval("(list 1 2)"), teval("(rest (list 0 1 2))"));
    }

    #[test]
    fn simple_func() {
        let mut env = Env::new();
        let _ = eval(&tokenize("(def 'f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
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
        assert_eq!(teval("'4"), teval("(do (+ 1 2) (+ 2 2))"));
    }

    #[test]
    fn simple_macro() {
        let mut env = Env::new();

        teval_env("(def 'm (macro () (+ 3 5))", &mut env).unwrap();
        assert_eq!(teval("'(+ 3 5)"), teval_env("(m)", &mut env).unwrap());
    }
}
