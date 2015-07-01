
fn eval_procedure(p: &Procedure, args: &[Expr], env: &mut Env) ->  ExprResult {
    try!(env.apply(&p.params, args));

    let body = SyntaxNode::Node(p.body.clone());
    let res = eval(&body, env);

    env.pop();

    res
}

fn default_env() -> Env {
    Env{def_map: vec![HashMap::new()]}
}

fn define(args: &[Expr], env: &mut Env) -> ExprResult {
    if args.len() != 2 {
        return Err(Error::InvalidArguments);
    }
    if let Expr::Atom(Atom::Symbol(ref key)) = args[0] {
        env.set(key.clone(), args[1].clone());
        Ok(Expr::Atom(Atom::Nil))
    } else {
        Err(Error::InvalidArguments)
    }
}

fn get(args: &[Expr], env: &Env) -> ExprResult {
    if args.len() != 1 {
        return Err(Error::InvalidArguments)
    }

    if let Expr::Atom(Atom::Symbol(ref s)) = args[0] {
        if let Some(a) = env.get(s) {
            Ok(a.clone())
        } else {
            Ok(Expr::Atom(Atom::Nil))
        }
    } else {
        Err(Error::InvalidArguments)
    }
}

fn eval_args(list: &[SyntaxNode], env: &mut Env) -> Result<Vec<Expr>, Error> {
    let mut args = vec![];

    for i in list.iter().skip(1) {
        let atom = try!(eval(i, env));
        args.push(atom);
    }
    Ok(args)
}

fn quote(v: &[Node]) -> Result<Expr, Error> {
    Ok(Expr::Node(v[1].clone()))
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

fn eval_node(atom: &Atom, list: &[SyntaxNode], env: &mut Env) -> ExprResult {
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

                    return Ok(Expr::Proc(prc))
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
                        Expr::Atom(Atom::Boolean(b)) => b,
                        Expr::Atom(Atom::Nil) => false,
                        _ => true
                    };

                    if truthy {
                        return eval(&list[2], env);
                    } else if list.len() > 2 {
                        return eval(&list[3], env);
                    } else {
                        return Ok(Expr::Atom(Atom::Boolean(false)))
                    }
                },
                _ => ()
            };

            let args = try!(eval_args(&list, env));

            if let Some(r) = try_built_ins(&s, &args, env) {
                return r
            }

            if let Some(&Expr::Proc(ref p)) = env.get(&s) {
                let mut renv = env.clone();
                let r = eval_procedure(p, &args, &mut renv);
                println!("evaling procedure, r = {:?}", r);
                return r
            }

            Ok(Expr::Atom(Atom::Nil))
        },
        _ => {
            println!("Atom is {:?}", atom);
            Err(Error::NotAFunction)
        }
    }
}

pub fn eval(p: &SyntaxNode, env: &mut Env) -> Result<Expr, Error> {
    match p {
        &SyntaxNode::Node(ref node) => {
            match node {
                &Node::List(ref list) => {
                    if list.is_empty() {
                        return Err(InvalidArguments)
                    }
                    match list[0] {
                        SyntaxNode::Node(Node::Atom(ref atom)) => {
                            println!("{:?} in here?", atom);
                            return eval_node(atom, list, env)
                        },
                        SyntaxNode::Node(Node::List(_)) => {
                            let r = try!(eval(&list[0], env));

                            match r {
                                Expr::Atom(a) => return eval_node(&a, list, env),
                                Expr::Proc(p) => return eval_procedure(&p, &try!(eval_args(&list, env)), env),
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
                                _ => return Ok(Expr::Atom(Atom::Nil))
                            }
                        }
                        _ => Ok(Expr::Atom(atom.clone()))
                    }
                }
            }
        },
        &SyntaxNode::Quote(ref node) => {
            match node {
                &Node::Atom(ref atom) => {
                    return Ok(Expr::Atom(atom.clone()))
                },
                &Node::List(ref list) => {
                    let mut out = VecDeque::with_capacity(list.len());
                    for i in list {
                        let n = try!(eval(i, env));

                        match n {
                            Expr::Atom(ref a) => {
                                out.push_back(a.clone());
                            }
                            _ => {
                                return Err(Error::InvalidArguments)
                            }
                        }
                    }
                    return Ok(Expr::Atom(Atom::List(out)))
                }
            }
            Ok(Expr::Node(node.clone()))
        }
        _ => panic!("argh")
    }
}

#[cfg(test)]
mod tests {
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

    fn make_atom_node(s: &str) -> SyntaxNode {
        return SyntaxNode::Node(Node::Atom(Atom::parse(s)));
    }

    fn as_list(n: &SyntaxNode) -> &Vec<SyntaxNode> {
        match n {
            &SyntaxNode::Node(Node::List(ref l)) => l,
            _ => panic!("don't get here!")
        }
    }

    fn as_atom(n: &SyntaxNode) -> &Atom {
        match n {
            &SyntaxNode::Node(Node::Atom(ref a)) => a,
            _ => panic!("not here!")
        }
    }

    fn atom(s: &str) -> Atom {
        Atom::parse(s)
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
        let _ = eval(&tokenize("(def 'f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
        let res = eval(&tokenize("(f 2 3)").unwrap(), &mut env).unwrap();

        assert_eq!(num(5), res);
    }

    #[test]
    fn quoting() {
        //assert_eq!("(a (+ 1 2) c)", "'(a (+ 1 2) c)"); 
    }
}
