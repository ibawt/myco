use expr::*;
use env::*;
use atom::*;
use errors::Error::*;
use number::*;

fn first(args: &[Expr], env: &Env) -> ExprResult {
    if let Some(p) = env.resolve_symbols(args).first() {
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

    match &env.resolve_symbols(args)[0] {
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

    for i in env.resolve_symbols(args) {
        match i {
            Expr::Atom(a) => out.push_back(a.clone()),
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Expr::Atom(Atom::List(out)))
}

pub fn try_built_ins(sym: &str, args: &[Expr], env: &Env) -> Option<ExprResult> {
    let result = match sym {
        "nil" => Ok(Expr::Atom(Atom::Nil)),
        "+" => add(args, env),
        "-" => sub(args, env),
        "=" => equals(args, env),
        ">=" => cmp(args, env, GreaterThanOrEqual),
        ">" => cmp(args, env, GreaterThan),
        "<=" => cmp(args, env, LessThanOrEqual),
        "<" => cmp(args, env, LessThan),
        "*" => mul(args, env),
        "/" => div(args,env),
        "first" => first(args,env),
        "rest" => rest(args,env),
        "list" => list(args,env),
        _ => return None
    };
    Some(result)
}
fn equals(args: &[Expr], env: &Env) -> ExprResult {
    if let Some(v) = env.resolve_symbols(args).first() {
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

    for i in env.resolve_symbols(v).iter() {
        match i {
            &Expr::Atom(Atom::Number(d)) => {
                result = result + d;
            },
            _ => return Err(UnexpectedType)
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

use self::Comparison::*;

fn cmp(v: &[Expr], env: &Env, cmp: Comparison) -> ExprResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = env.resolve_symbols(v);

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
        return Err(InvalidArguments)
    }

    let vv = env.resolve_symbols(v);

    let mut result = match vv[0] {
        Expr::Atom(Atom::Number(d)) => d,
        _ => return Err(UnexpectedType)
    };

    for i in vv.iter().skip(1) {
        match i {
            &Expr::Atom(Atom::Number(d)) => result = result - d,
            _ => return Err(UnexpectedType)
        }
    }

    Ok(Expr::Atom(Atom::Number(result)))
}

fn mul(v: &[Expr], env: &Env) -> ExprResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = env.resolve_symbols(v);

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

    let vv = env.resolve_symbols(v);

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
