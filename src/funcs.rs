use std::io::prelude::*;
use std::fs::File;

use env::*;
use atom::*;
use errors::Error::*;
use number::*;
use eval;

fn cons(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() != 2 {
        return Err(InvalidArguments)
    }
    if let Atom::Nil = args[1] {
        return Ok(Atom::list(vec![args[0].clone()]));
    }

    let list = try!(args[1].as_list());

    let mut v = Vec::with_capacity(list.len() + 1);
    v.push(args[0].clone());
    for i in list.as_ref() {
        v.push(i.clone());
    }
    Ok(Atom::list(v))
}

fn append(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() != 2 {
        return Err(InvalidArguments)
    }

    let ref a = args[0];
    let ref b = args[1];

    match (a, b) {
        (&Atom::List(ref a), &Atom::List(ref b)) => {
            Ok(Atom::list(a.iter().chain(b.iter()).map(|n| n.clone()).collect()))
        },
        (&Atom::List(ref a), &Atom::Nil) => {
            Ok(Atom::list(a.iter().map(|n| n.clone()).collect()))
        }
        (_, &Atom::Nil) => {
            Ok(Atom::list(vec![a.clone()]))
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
        if let Atom::Nil = *p {
            return Ok(Atom::Nil)
        }
        let list = try!(p.as_list());
        if let Some(f) = list.first() {
            Ok(f.clone())
        } else {
            Ok(Atom::Nil)
        }
    } else {
        Err(NotEnoughArguments)
    }
}

fn rest(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() < 1 {
        return Err(NotEnoughArguments)
    }
    if let Atom::Nil = args[0] {
        return Ok(Atom::Nil)
    }
    let list = try!(args[0].as_list());
    Ok(Atom::list(list.iter().skip(1).map(|x| x.clone()).collect()))
}

fn list(args: &[Atom], _: &Env) -> AtomResult {
    Ok(Atom::list(args.iter().map(|n| n.clone()).collect()))
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

fn type_of(args: &[Atom], _: &Env) -> AtomResult {
    if let Some(first) = args.first() {
        let s = match *first {
            Atom::List(_) => "list",
            Atom::Number(Number::Integer(_)) => "integer",
            Atom::Number(Number::Float(_)) => "float",
            Atom::Keyword(_) => "keyword",
            Atom::String(_) => "string",
            Atom::Symbol(_) => "symbol",
            Atom::Boolean(_) => "bool",
            Atom::Function(_) => "function",
            Atom::Form(_) => "special_form",
            Atom::Nil => "nil"
        };

        Ok(Atom::symbol(s))
    } else {
        Err(NotEnoughArguments)
    }
}


fn apply(f: &Function, args: &[Atom], env: &mut Env) -> AtomResult {
    match *f {
        Function::Proc(ref p) => {
            let mut e = Env::new(Some(p.closures.clone())).bind(&p.params, args);

            eval::eval(Atom::List(p.body.clone()), &mut e)
        },
        Function::Native(func) => {
            eval_native(func, args, env)
        },
        _ => return Err(InvalidArguments)
    }
}

fn map(args: &[Atom], env: &mut Env) -> AtomResult {
    let list = try!(args[1].as_list());
    let func = try!(args[0].as_function());

    let mut out = Vec::with_capacity(list.len());
    for i in 0..list.len() {
        out.push(try!(apply(func, &list[i..], env)));
    }
    Ok(Atom::list(out))
}

fn filter(arg: &[Atom], env: &mut Env) -> AtomResult {
    let list = try!(arg[1].as_list());
    let func = try!(arg[0].as_function());

    let mut out = Vec::with_capacity(list.len());

    for i in 0..list.len() {
        if try!(apply(func, &list[i..], env)).as_bool() {
            out.push(list[i].clone());
        }
    }

    out.shrink_to_fit();

    Ok(Atom::list(out))
}

fn count(args: &[Atom], _: &mut Env) -> AtomResult {
    match args[0] {
        Atom::List(ref l) => {
            Ok(Atom::Number(Number::Integer(l.len() as i64)))
        },
        Atom::Nil => {
            Ok(Atom::Number(Number::Integer(0)))
        },
        _ => Err(UnexpectedType)
    }
}

fn reduce(args: &[Atom], env: &mut Env) -> AtomResult {
    let list = try!(args[1].as_list());
    let func = try!(args[0].as_function());
    let mut acc = args[2].clone();

    let mut args = vec![];

    for i in 0..list.len() {
        args.clear();
        args.push(acc);
        args.push(list[i].clone());

        acc = try!(apply(func, &args, env));
    }

    Ok(acc)
}

fn slurp(args: &[Atom], _: &mut Env) -> AtomResult {
    let filename = try!(args[0].as_string());

    let mut file = try!(File::open(filename));
    let mut s = String::new();
    try!(file.read_to_string(&mut s));

    Ok(Atom::String(s))
}

fn barf(args: &[Atom], _: &mut Env) -> AtomResult {
    let filename = try!(args[0].as_string());
    let contents = try!(args[1].as_string());
    let mut file = try!(File::create(filename));
    try!(file.write_all(contents.as_bytes()));

    Ok(Atom::Boolean(true))
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
        Print => print(args, env),
        Error => error(args, env),
        Type => type_of(args, env),
        Map => map(args, env),
        Slurp => slurp(args, env),
        Barf => barf(args, env),
        Filter => filter(args, env),
        Reduce => reduce(args, env),
        Count => count(args, env)
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

    for i in v {
        result = result + try!(i.as_number());
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

fn error(_: &[Atom], _: &Env) -> AtomResult {
    Err(RuntimeAssertion)
}

fn cmp(v: &[Atom], _: &Env, cmp: Comparison) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let initial = try!(v[0].as_number());
    for i in v.iter().skip(1) {
        let d = try!(i.as_number());
        let b = match cmp {
            LessThan => initial < d,
            GreaterThan => initial > d,
            LessThanOrEqual => initial <= d,
            GreaterThanOrEqual => initial >= d
        };

        if !b {
            return Ok(Atom::Boolean(false))
        }
    }
    Ok(Atom::Boolean(true))
}

fn sub(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 1 {
        return Err(InvalidArguments)
    }

    let mut result = try!(v[0].as_number());
    for i in v.iter().skip(1) {
        let d = try!(i.as_number());
        result = result - d;
    }

    Ok(Atom::Number(result))
}

fn mul(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let mut initial = try!(v[0].as_number());
    for i in v.iter().skip(1) {
        let d = try!(i.as_number());
        initial = initial * d;
    }
    Ok(Atom::Number(initial))
}

fn div(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let mut initial = try!(v[0].as_number());

    for i in v.iter().skip(1) {
        let d = try!(i.as_number());
        if d.is_zero() {
            return Err(RuntimeAssertion)
        }
        initial = initial / d;
    }
    Ok(Atom::Number(initial))
}

