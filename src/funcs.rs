use env::*;
use atom::*;
use errors::Error::*;
use number::*;

fn cons(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() != 2 {
        return Err(InvalidArguments)
    }

    match args[1] {
        Atom::List(ref list) => {
            let mut v = vec![args[0].clone()];
            for i in list {
                v.push(i.clone());
            }
            Ok(Atom::List(v))
        }
        _ => {
            Err(InvalidArguments)
        }
    }
}

fn append(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() != 2 {
        return Err(InvalidArguments)
    }

    let ref a = args[0];
    let ref b = args[1];

    match (a, b) {
        (&Atom::List(ref a), &Atom::List(ref b)) => {
            Ok(Atom::List(a.iter().chain(b.iter()).map(|n| n.clone()).collect()))
        },
        (&Atom::List(ref a), &Atom::Nil) => {
            Ok(Atom::List(a.iter().map(|n| n.clone()).collect()))
        }
        (_, &Atom::Nil) => {
            Ok(Atom::List(vec![a.clone()]))
        }
        _ => {
            Err(InvalidArguments)
        }
    }
}

fn print(args: &[Atom], _: &Env) -> AtomResult {
    for i in args {
        print!("{} ", i);
    }
    print!("\n");

    Ok(Atom::Nil)
}

fn first(args: &[Atom], _: &Env) -> AtomResult {
    if let Some(p) = args.first() {
        match *p {
            Atom::List(ref list) => {
                if let Some(f) = list.first() {
                    Ok(f.clone())
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

fn rest(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() < 1 {
        return Err(NotEnoughArguments)
    }

    match args[0] {
        Atom::List(ref list) => {
            Ok(Atom::List(list.iter().skip(1).map(|x| x.clone()).collect()))
        },
        _ => Err(InvalidArguments)
    }
}

fn list(args: &[Atom], _: &Env) -> AtomResult {
    Ok(Atom::List(args.iter().map(|n| n.clone()).collect()))
}

fn not(args: &[Atom], _: &Env) -> AtomResult {
    args.first().ok_or(NotEnoughArguments)
        .map(|a|
             Atom::from(match *a {
                 Atom::Boolean(b) => !b,
                 Atom::Nil => true,
                 _ => false
             }))
}

pub fn eval_native(n: Native, args: &[Atom], env: &mut Env) -> AtomResult {
    use atom::Native::*;
    match n {
        Add => add(args, env),
        Sub => sub(args, env),
        Equal => equals( args, env),
        GreaterThanOrEqual => cmp(args, env, Comparison::GreaterThanOrEqual),
        GreaterThan => cmp(args, env, Comparison::GreaterThan),
        LessThanOrEqual => cmp(args, env, Comparison::LessThanOrEqual),
        LessThan => cmp(args, env, Comparison::LessThan),
        Mul => mul(args, env),
        Div => div(args, env),
        First => first(args, env),
        Rest => rest(args, env),
        Not => not(args, env),
        List => list(args, env),
        Cons => cons(args, env),
        Append => append(args, env),
        Print => print(args, env)
    }
}

fn equals(args: &[Atom], _: &Env) -> AtomResult {
    if let Some(v) = args.first() {
        for i in args.iter().skip(1) {
            if v != i {
                return Ok(Atom::Boolean(false))
            }
        }
        Ok(Atom::Boolean(true))
    } else {
        Err(NotEnoughArguments)
    }
}

fn add(v: &[Atom], _: &Env) -> AtomResult {
    let mut result = Number::Integer(0);

    for i in v.iter() {
        match *i {
            Atom::Number(d) => {
                result = result + d;
            },
            _ => {
                // println!("I got a {:?} instead of a number!", i);
                return Err(UnexpectedType)
            }
        }
    }

    Ok(Atom::Number(result))
}

enum Comparison {
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual
}

use self::Comparison::*;

fn cmp(v: &[Atom], _: &Env, cmp: Comparison) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let initial = match v[0] {
        Atom::Number(a) => a,
        _ => {
            return Err(UnexpectedType)
        }
    };

    for i in v.iter().skip(1) {
        match *i {
            Atom::Number(d) => {
                let b = match cmp {
                    LessThan => initial < d,
                    GreaterThan => initial > d,
                    LessThanOrEqual => initial <= d,
                    GreaterThanOrEqual => initial >= d
                };

                if !b {
                    return Ok(Atom::Boolean(false))
                }
            },
            _ => {
                // println!("wayyyy over here");
                return Err(UnexpectedType)
            }
        }
    }

    Ok(Atom::Boolean(true))
}

fn sub(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 1 {
        return Err(InvalidArguments)
    }

    let mut result = match v[0] {
        Atom::Number(d) => d,
        _ => {
            // println!("over here");
            return Err(UnexpectedType)
        }
    };

    for i in v.iter().skip(1) {
        match *i {
            Atom::Number(d) => result = result - d,
            _ => {
                // println!("right here");
                return Err(UnexpectedType)
            }
        }
    }

    Ok(Atom::Number(result))
}

fn mul(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let mut initial = match v[0] {
        Atom::Number(d) => d,
        _ => return Err(InvalidArguments)
    };

    for i in v.iter().skip(1) {
        match *i {
            Atom::Number(d) => initial = initial * d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Atom::Number(initial))
}

fn div(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let mut initial = match v[0] {
        Atom::Number(d) => d,
        _ => return Err(InvalidArguments)
    };

    for i in v.iter().skip(1) {
        match *i {
            Atom::Number(d) => initial = initial / d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Atom::Number(initial))
}

