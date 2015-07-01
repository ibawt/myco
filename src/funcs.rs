fn first(args: &[Expr], env: &Env) -> ExprResult {
    if let Some(p) = resolve_symbols(args,env).first() {
        match p {
            &Expr::Atom(Atom::List(ref list)) => {
                if let Some(f) = list.front() {
                    Ok(Expr::Atom(f.clone()))
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

fn rest(args: &[Expr], env: &Env) -> ExprResult {
    if args.len() < 1 {
        return Err(NotEnoughArguments)
    }

    match &resolve_symbols(args,env)[0] {
        &Expr::Atom(Atom::List(ref list)) => {
            if list.len() > 1 {
                let mut out = VecDeque::with_capacity(list.len() - 1);

                for i in list.iter().skip(1) {
                    out.push_back(i.clone());
                }
                Ok(Expr::Atom(Atom::List(out)))
            } else {
                Ok(Expr::Atom(Atom::List(list.clone())))
            }
        },
        _ => Err(InvalidArguments)
    }
}

fn list(args: &[Expr], env: &Env) -> ExprResult {
    let mut out =  VecDeque::with_capacity(args.len());

    for i in resolve_symbols(args, env) {
        match i {
            Expr::Atom(a) => out.push_back(a.clone()),
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Expr::Atom(Atom::List(out)))
}

pub fn try_built_ins(sym: &str, args: &[Expr], env: &Env) -> Option<ExprResult> {
    match sym {
        "nil" => Some(Ok(Expr::Atom(Atom::Nil))),
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
fn equals(args: &[Expr], env: &Env) -> ExprResult {
    if let Some(v) = resolve_symbols(args,env).first() {
        for i in args.iter().skip(1) {
            if v != i {
                return Ok(Expr::Atom(Atom::Boolean(false)))
            }
        }
        Ok(Expr::Atom(Atom::Boolean(true)))
    } else {
        Err(NotEnoughArguments)
    }
}

fn add(v: &[Expr], env: &Env) -> ExprResult {
    let mut result = Number::Integer(0);

    for i in resolve_symbols(v, env).iter() {
        match i {
            &Expr::Atom(Atom::Number(d)) => {
                result = result + d;
            },
            _ => return Err(Error::UnexpectedType)
        }
    }

    Ok(Expr::Atom(Atom::Number(result)))
}

enum Comparison {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual
}

use Comparison::*;

fn cmp(v: &[Expr], env: &Env, cmp: Comparison) -> ExprResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = resolve_symbols(v, env);

    let initial = match vv[0] {
        Expr::Atom(Atom::Number(a)) => a,
        _ => return Err(UnexpectedType)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Expr::Atom(Atom::Number(d)) => {
                let b = match cmp {
                    LessThan => initial < d,
                    GreaterThan => initial > d,
                    LessThanOrEqual => initial <= d,
                    GreaterThanOrEqual => initial >= d
                };

                if !b {
                    return Ok(Expr::Atom(Atom::Boolean(false)))
                }
            },
            _ => return Err(UnexpectedType)
        }
    }

    Ok(Expr::Atom(Atom::Boolean(true)))
}

fn sub(v: &[Expr], env: &Env) -> ExprResult {
    if v.len() < 1 {
        return Err(Error::InvalidArguments)
    }

    let vv = resolve_symbols(v, env);

    let mut result = match vv[0] {
        Expr::Atom(Atom::Number(d)) => d,
        _ => return Err(Error::UnexpectedType)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Expr::Atom(Atom::Number(d)) => result = result - d,
            _ => return Err(Error::UnexpectedType)
        }
    }

    Ok(Expr::Atom(Atom::Number(result)))
}

fn mul(v: &[Expr], env: &Env) -> ExprResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = resolve_symbols(v, env);

    let mut initial = match vv[0] {
        Expr::Atom(Atom::Number(d)) => d,
        _ => return Err(InvalidArguments)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Expr::Atom(Atom::Number(d)) => initial = initial * d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Expr::Atom(Atom::Number(initial)))
}

fn div(v: &[Expr], env: &Env) -> ExprResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = resolve_symbols(v, env);

    let mut initial = match vv[0] {
        Expr::Atom(Atom::Number(d)) => d,
        _ => return Err(InvalidArguments)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Expr::Atom(Atom::Number(d)) => initial = initial / d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Expr::Atom(Atom::Number(initial)))
}
