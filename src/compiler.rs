use atom::*;
use eval::*;
use env::*;
use errors::*;
use opcodes::*;
use opcodes::Opcode::*;

fn expand_quasiquote(node: &Atom, env: &Env) -> AtomResult {
    if !node.is_pair() {
        return Ok(Atom::list(vec![Atom::Form(Form::Quote), node.clone()]));
    }

    let list = try!(node.as_list());
    if let Atom::Form(Form::Unquote) = list[0] {
        try!(expect_arg_length(list, 2));
        return Ok(list[1].clone());
    }

    if list[0].is_pair() {
        if let Atom::List(ref sublist) = list[0] {
            if let Atom::Form(Form::Splice) = sublist[0] {
                let append = Atom::Function(Function::Native(Native::Append));
                let rest = list[1..].iter().cloned().collect();
                return Ok(Atom::list(vec![append, sublist[1].clone(), Atom::list(rest)]));
            }
        }
    }

    let rest: Vec<Atom> = list[1..].iter().cloned().collect();

    if rest.is_empty() {
        Ok(Atom::list(vec![try!(expand_quasiquote(&list[0], env))]))
    } else {
        Ok(Atom::list(vec![Atom::Function(Function::Native(Native::Cons)),
                           try!(expand_quasiquote(&list[0], env)),
                           try!(expand_quasiquote(&Atom::list(rest), env))]))
    }
}

fn macro_expand(node: Atom, env: &mut Env) -> Result<Atom, Error> {
    match node {
        Atom::List(ref list) if !list.is_empty() => {
            match list[0] {
                Atom::Symbol(ref sym) => {
                    if let Some(Atom::Function(Function::Macro(ref m))) = env.get(sym.as_ref()) {
                        return macro_expand(try!(eval_macro(m, &list[1..], env)), env);
                    }
                }
                Atom::Form(Form::Fn) => {
                    if list.len() < 3 {
                        return Err(Error::NotEnoughArguments);
                    }

                    let mut out = Vec::with_capacity(list.len());
                    out.push(list[0].clone());
                    out.push(list[1].clone());
                    for i in &list[2..] {
                        out.push(try!(macro_expand(i.clone(), env)));
                    }
                    return Ok(Atom::list(out));
                }
                _ => (),
            }

            return Ok(Atom::list(try!(list.iter()
                .map(|n| macro_expand(n.clone(), env))
                .collect())));
        }
        _ => (),
    }

    Ok(node)
}

fn compile_node(node: Atom, out: &mut Vec<Opcode>, env: &mut Env) -> Result<(), Error> {
    match node {
        Atom::Symbol(sym) => out.push(Opcode::LOAD(sym)),
        Atom::List(ref list) => {
            for n in list.iter() {
                try!(compile(n.clone(), out, env));
            }
        }
        _ => {
            out.push(Opcode::CONST(node.clone()));
        }
    }
    Ok(())
}

fn compile_form(form: Form,
                list: &List,
                out: &mut Vec<Opcode>,
                env: &mut Env)
                -> Result<(), Error> {
    match form {
        Form::Recur => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            out.push(RECUR(list.len() - 1));
        }
        Form::If => {
            match list.len() {
                4 => {
                    // if with ELSE
                    try!(compile(list[1].clone(), out, env));
                    out.push(JUMP_IFNOT(0));
                    let else_jump_pos = out.len() - 1;
                    try!(compile(list[2].clone(), out, env));

                    out.push(JUMP(0));
                    let out_jump = out.len() - 1;

                    out[else_jump_pos] = JUMP_IFNOT(out.len());

                    try!(compile(list[3].clone(), out, env));

                    out[out_jump] = JUMP(out.len());
                }
                3 => {
                    // no ELSE
                    try!(compile(list[1].clone(), out, env));
                    out.push(JUMP_IFNOT(0));
                    let else_jump_pos = out.len() - 1;
                    try!(compile(list[2].clone(), out, env));
                    out[else_jump_pos] = JUMP_IFNOT(out.len());
                }
                _ => return Err(invalid_arg("invalid number for forms for if")),
            }
        }
        Form::Let => {
            let new_env = Env::new(Some(env.clone()));
            let bind_list = try!(list[1].as_list());
            let mut bindings = Vec::new();

            for binding in bind_list.iter() {
                let bind_exp = try!(binding.as_list());
                try!(expect_arg_length(bind_exp, 2));
                bindings.push(bind_exp[0].clone());
                try!(compile(bind_exp[1].clone(), out, env));
            }

            let mut body = Vec::new();

            try!(compile(list[2].clone(), &mut body, env));

            body.push(RETURN);

            let arity = bindings.len();

            let func = CompiledFunction {
                body: body,
                source: list.clone(),
                params: to_list(bindings),
                env: new_env,
            };

            out.push(CONST(Atom::Function(Function::Compiled(func))));
            out.push(DCALL(arity));
        }
        Form::Set => {
            let sym = try!(list[1].as_symbol());
            try!(compile(list[2].clone(), out, env));
            out.push(STORE(*sym));
        }
        Form::Def => {
            let sym = try!(list[1].as_symbol());
            try!(compile(list[2].clone(), out, env));
            out.push(DEFINE(*sym));
        }
        Form::Do => {
            if list.len() > 2 {
                for i in list.iter().take(list.len() - 1).skip(1) {
                    try!(compile(i.clone(), out, env));
                    out.push(POP);
                }
            }
            return compile(list[list.len() - 1].clone(), out, env);
        }
        Form::Fn => {
            let mut body = Vec::new();
            let mut env = Env::new(Some(env.clone()));
            try!(compile(list[2].clone(), &mut body, &mut env));
            body.push(RETURN);
            let func = CompiledFunction {
                body: body,
                source: list.clone(),
                params: try!(list[1].as_list()).clone(),
                env: env,
            };
            out.push(CONST(Atom::Function(Function::Compiled(func))));
        }
        Form::Macro => {
            try!(macro_form(&list[1..], env));
            out.push(CONST(Atom::Nil));
        }
        Form::Quote => {
            out.push(CONST(list[1].clone()));
        }
        Form::QuasiQuote => {
            let expanded = try!(expand_quasiquote(&list[1], env));
            try!(compile(expanded, out, env));
        }
        _ => return Err(Error::NotImplemented),
    }
    Ok(())
}

pub fn compile(node: Atom, out: &mut Vec<Opcode>, env: &mut Env) -> Result<(), Error> {
    match node {
        Atom::List(ref list) => {
            if list.is_empty() {
                out.push(Opcode::CONST(node.clone()));
                return Ok(());
            }
            ()
        }
        _ => return compile_node(node, out, env),
    }

    let list = match try!(macro_expand(node, env)) {
        Atom::List(list) => {
            if list.is_empty() {
                out.push(CONST(Atom::Nil));
                return Ok(());
            } else {
                list
            }
        }
        c => {
            out.push(CONST(c));
            return Ok(());
        }
    };

    if let Atom::List(_) = list[0] {
        for i in list.iter().skip(1) {
            try!(compile(i.clone(), out, env));
        }
        try!(compile(list[0].clone(), out, env));
        out.push(DCALL(list.len() - 1));
        return Ok(());
    }

    match list[0] {
        Atom::Form(f) => return compile_form(f, &list, out, env),
        Atom::Symbol(_) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            try!(compile_node(list[0].clone(), out, env));
            out.push(DCALL(list.len() - 1));
            return Ok(());
        }
        Atom::Function(ref func) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            match *func {
                Function::Native(Native::Apply) => {
                    out.push(Opcode::APPLY);
                    return Ok(());
                }
                Function::Native(_) => {
                    out.push(Opcode::CALL(func.clone(), list.len() - 1));
                    return Ok(());
                }
                Function::Compiled(_) => {
                    out.push(CALL(func.clone(), list.len() - 1));
                    return Ok(());
                }
                _ => return Err(Error::NotImplemented),
            }
        }
        _ => return Err(Error::NotAFunction),
    }

    Err(invalid_arg("bottom here"))
}
