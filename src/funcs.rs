//////////////////////////////////////////////////////////////////////////////
// Copyright 2015 Ian Quick <ian.quick@gmail.com>                           //
//                                                                          //
// Licensed under the Apache License, Version 2.0 (the "License");          //
// you may not use this file except in compliance with the License.         //
// You may obtain a copy of the License at                                  //
//                                                                          //
//   http://www.apache.org/licenses/LICENSE-2.0                             //
//                                                                          //
// Unless required by applicable law or agreed to in writing, software      //
// distributed under the License is distributed on an "AS IS" BASIS,        //
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. //
// See the License for the specific language governing permissions and      //
// limitations under the License.                                           //
//////////////////////////////////////////////////////////////////////////////
use env::*;
use atom::*;
use errors::Error::*;
use number::*;

fn first(args: &[Atom], env: &Env) -> AtomResult {
    if let Some(p) = env.resolve_symbols(args).first() {
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

fn rest(args: &[Atom], env: &Env) -> AtomResult {
    if args.len() < 1 {
        return Err(NotEnoughArguments)
    }

    match env.resolve_symbols(args)[0] {
        Atom::List(ref list) => {
            Ok(Atom::List(list.iter().skip(1).map(|x| x.clone()).collect()))
        },
        _ => Err(InvalidArguments)
    }
}

fn list(args: &[Atom], env: &Env) -> AtomResult {
    Ok(Atom::List(env.resolve_symbols(args).iter().map(|n| n.clone()).collect()))
}

fn not(args: &[Atom], env: &Env) -> AtomResult {
    env.resolve_symbols(args).first().ok_or(NotEnoughArguments)
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
        List => list(args, env)
    }
}

fn equals(args: &[Atom], env: &Env) -> AtomResult {
    if let Some(v) = env.resolve_symbols(args).first() {
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

fn add(v: &[Atom], env: &Env) -> AtomResult {
    let mut result = Number::Integer(0);

    for i in env.resolve_symbols(v).iter() {
        match *i {
            Atom::Number(d) => {
                result = result + d;
            },
            _ => return Err(UnexpectedType)
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

fn cmp(v: &[Atom], env: &Env, cmp: Comparison) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = env.resolve_symbols(v);

    let initial = match vv[0] {
        Atom::Number(a) => a,
        _ => return Err(UnexpectedType)
    };

    for i in vv.iter().skip(1) {
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
            _ => return Err(UnexpectedType)
        }
    }

    Ok(Atom::Boolean(true))
}

fn sub(v: &[Atom], env: &Env) -> AtomResult {
    if v.len() < 1 {
        return Err(InvalidArguments)
    }

    let vv = env.resolve_symbols(v);

    let mut result = match vv[0] {
        Atom::Number(d) => d,
        _ => return Err(UnexpectedType)
    };

    for i in vv.iter().skip(1) {
        match *i {
            Atom::Number(d) => result = result - d,
            _ => return Err(UnexpectedType)
        }
    }

    Ok(Atom::Number(result))
}

fn mul(v: &[Atom], env: &Env) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = env.resolve_symbols(v);

    let mut initial = match vv[0] {
        Atom::Number(d) => d,
        _ => return Err(InvalidArguments)
    };

    for i in vv.iter().skip(1) {
        match *i {
            Atom::Number(d) => initial = initial * d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Atom::Number(initial))
}

fn div(v: &[Atom], env: &Env) -> AtomResult {
    if v.len() < 2 {
        return Err(NotEnoughArguments)
    }

    let vv = env.resolve_symbols(v);

    let mut initial = match vv[0] {
        Atom::Number(d) => d,
        _ => return Err(InvalidArguments)
    };

    for i in vv.iter().skip(1) {
        match *i {
            Atom::Number(d) => initial = initial / d,
            _ => return Err(InvalidArguments)
        }
    }
    Ok(Atom::Number(initial))
}
