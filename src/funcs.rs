use std::io::prelude::*;
use std::fs::File;

use env::*;
use atom::*;
use errors::*;
use number::*;
use eval;

use errors::Error::*;

fn cons(args: &[Atom], _: &Env) -> AtomResult {
    if args.len() != 2 {
        return Err(invalid_arg("cons"));
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
        return Err(invalid_arg("append"));
    }

    let a = &args[0];
    let b = &args[1];

    match (a, b) {
        (&Atom::List(ref a), &Atom::List(ref b)) => {
            Ok(Atom::list(a.iter().chain(b.iter()).cloned().collect()))
        }
        (&Atom::List(ref a), &Atom::Nil) => Ok(Atom::list(a.iter().cloned().collect())),
        (_, &Atom::Nil) => Ok(Atom::list(vec![a.clone()])),
        (_, &Atom::List(ref l)) => {
            let mut list: Vec<Atom> = l.iter().cloned().collect();
            list.push(a.clone());

            Ok(Atom::list(list))
        }
        _ => Err(invalid_arg("append")),
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
            return Ok(Atom::Nil);
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
        return Err(NotEnoughArguments);
    }
    if let Atom::Nil = args[0] {
        return Ok(Atom::Nil);
    }
    let list = try!(args[0].as_list());
    Ok(Atom::list(list.iter().skip(1).cloned().collect()))
}

fn list(args: &[Atom], _: &Env) -> AtomResult {
    Ok(Atom::list(args.iter().cloned().collect()))
}

fn not(args: &[Atom], _: &Env) -> AtomResult {
    args.first()
        .ok_or(NotEnoughArguments)
        .map(|a| {
            Atom::from(match *a {
                Atom::Boolean(b) => !b,
                Atom::Nil => true,
                _ => false,
            })
        })
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
            Atom::Nil => "nil",
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

            eval::eval(Atom::List(p.body.clone()), &mut e, None)
        }
        Function::Native(func) => eval_native(func, args, env),
        _ => Err(invalid_arg("apply")),
    }
}

fn count(args: &[Atom], _: &mut Env) -> AtomResult {
    match args[0] {
        Atom::List(ref l) => Ok(Atom::Number(Number::Integer(l.len() as i64))),
        Atom::Nil => Ok(Atom::Number(Number::Integer(0))),
        _ => Err(UnexpectedType),
    }
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

fn get(args: &[Atom], _: &mut Env) -> AtomResult {
    if args.len() < 2 {
        return Err(invalid_arg("get takes 2 args"));
    }
    let list = try!(args[0].as_list());
    let ord = try!(args[1].as_number()).as_integer();
    list.get(ord as usize).cloned().ok_or_else(|| invalid_arg("ord is out of range"))
}

pub fn eval_native(n: Native, args: &[Atom], env: &mut Env) -> AtomResult {
    use atom::Native::*;
    for i in args {
        println!("ARG - {}", i)
    }
    match n {
        Add => add(args, env),
        Sub => sub(args, env),
        Equal => equals(args, env),
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
        Slurp => slurp(args, env),
        Barf => barf(args, env),
        Count => count(args, env),
        Apply => apply(try!(args[1].as_function()), &args[1..], env),
        Get => get(args, env),
    }
}

fn equals(args: &[Atom], _: &Env) -> AtomResult {
    if let Some(v) = args.first() {
        for i in args.iter().skip(1) {
            if v != i {
                return Ok(Atom::Boolean(false));
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
    GreaterThanOrEqual,
}

use self::Comparison::*;

fn error(_: &[Atom], _: &Env) -> AtomResult {
    Err(RuntimeAssertion)
}

fn cmp(v: &[Atom], _: &Env, cmp: Comparison) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments);
    }

    let initial = try!(v[0].as_number());
    for i in v.iter().skip(1) {
        let d = try!(i.as_number());
        let b = match cmp {
            LessThan => initial < d,
            GreaterThan => initial > d,
            LessThanOrEqual => initial <= d,
            GreaterThanOrEqual => initial >= d,
        };

        if !b {
            return Ok(Atom::Boolean(false));
        }
    }
    Ok(Atom::Boolean(true))
}

fn sub(v: &[Atom], _: &Env) -> AtomResult {
    if v.len() < 1 {
        return Err(invalid_arg("sub"));
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
        return Err(NotEnoughArguments);
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
        return Err(NotEnoughArguments);
    }

    let mut initial = try!(v[0].as_number());

    for i in v.iter().skip(1) {
        let d = try!(i.as_number());
        if d.is_zero() {
            return Err(RuntimeAssertion);
        }
        initial = initial / d;
    }
    Ok(Atom::Number(initial))
}
