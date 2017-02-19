use atom::*;
use eval::*;
use env::*;
use errors::*;
use opcodes::*;
use opcodes::Opcode::*;
use symbol;
use parser;


fn next_symbol(prefix: char, n: u32) -> Atom {
    let s = format!("{}{}", prefix, n);
    Atom::symbol(&s)
}

use std::rc::Rc;

fn cps_wrap_native(n: Native, args: &[Atom], cont: Atom) -> Vec<Atom> {
    let f = Atom::Function(Function::Continuation(NativeContinuation{
        native: n
    }));

    let mut v = vec![f];
    for a in args {
        v.push(a.clone());
    }
    v.push(cont);
    v
}

fn split_args(n: &[Atom]) -> (Vec<Atom>, Vec<Atom>) {
    let mut fns = vec![];
    let mut literals = vec![];

    for i in n {
        match *i {
            Atom::List(_) => {
                fns.push(i.clone());
            },
            _ => {
                literals.push(i.clone());
            }
        }
    }

    println!("{} splits fns: {}, literals: {}", print_list(n),
             print_list(&fns), print_list(&literals));

    (fns, literals)
}

pub fn cps_translate(node: Atom, cont: Atom, symbol_count: u32) -> Result<Atom, Error> {
    println!("cps_translate: {}, cont = {}", node, cont);
    if let Atom::List(ref list) = node {
        match list[0] {
            Atom::Function(Function::Native(n)) => {
                let native = Atom::Function(Function::Continuation(NativeContinuation{
                    native: n
                }));
                // Example
                // Input: (+ (+ 1 2) 1)
                // Output: (+/k 1 2 (fn (a0) (+/k 1 a0 k)))

                // Input: (+ 1 2)
                // Output: (+/k 1 2 k)

                // Input: (+ (+ 1 2) (+ 3 4) 5)
                // Output: (+/k 1 2 (fn (a0) (+/k 3 4 (fn (a1) (+/k a0 a1 5 k)))))

                let (fns, literals) = split_args(&list[1..]);

                if fns.is_empty() {
                    let mut x = vec![native];
                    for i in literals {
                        x.push(i);
                    }
                    x.push(cont);
                    Ok(Atom::list(x))
                } else {
                    // Wrap myself
                    let mut args = vec![];
                    for (i, _) in fns.iter().enumerate() {
                        args.push(next_symbol('a', symbol_count + i as u32));
                    }
                    let mut body = vec![native];
                    for a in &args {
                        body.push(a.clone());
                    }

                    for i in &literals {
                        body.push(i.clone());
                    }

                    body.push(cont);

                    let mut current = Atom::list(vec![ Atom::Form(Form::Fn),
                                              Atom::list(vec![args[0].clone()]),
                                              Atom::list(body)]);

                    for (i, func) in fns.into_iter().enumerate() {
                        current = cps_translate(func, current, symbol_count + i as u32)?;

                        if i < (args.len()-1) {
                            current = Atom::list(vec![Atom::Form(Form::Fn),
                                                      Atom::list(vec![args[i+1].clone()]),
                                                      current]);
                        }
                    }

                    Ok(current)
                    // let mut body = vec![native];
                    // for i in &arg_vars {
                    //     body.push(i.clone());
                    // }
                    // body.push(cont);

                    // let mut x = Atom::list(vec![ Atom::Form(Form::Fn),
                    //                              Atom::list(vec![arg_vars.last().unwrap().clone()]),
                    //                              Atom::list(body)]);
                    // for i in 0..arg_vars.len() {
                    //     x = try!(cps_translate(fns[i].clone(), x, symbol_count + 1));

                    //     if i > 0 {
                    //         let ff = Atom::list(vec![Atom::Form(Form::Fn),
                    //                                  Atom::list(vec![arg_vars[i].clone()]), x]);
                    //         x = ff;
                    //         println!("x = {}", x);
                    //     }
                    // }
                    // return Ok(x)
                }
            }
            _ => {
                panic!("invalid function call!")
            }
        }
    } else {
        Ok(node)
    }
}

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
    trace!("macro_expand: {}", &node);
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
        Form::Eval => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            out.push(EVAL);
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
                id: 0,
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
                id: 0,
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

    trace!("after macro expansion: {}", print_list(&list));

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
                },
                Function::Continuation(n) => {
                    out.push(Opcode::CALL(func.clone(), list.len() - 1));
                    return Ok(())
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

#[cfg(test)]
mod tests {
    use super::*;
    use parser;
    use atom::*;
    use std::rc::Rc;
    use env::Env;
    use eval::print_list;
    use vm;

    use super::next_symbol;

    fn run_node(node: Atom) -> AtomResult {
        let mut vm = vm::VirtualMachine::default();
        vm.run_node(node)
    }

    fn t(s: &str) -> Atom {
        parser::tokenize_single(s).unwrap()
    }

    fn string_equals(a: &Atom, b: &Atom) {
        let aa = format!("{}", a);
        let bb = format!("{}", b);

        println!("EXPECTED = {}", aa);
        println!("ACTUAL   = {}", bb);
        assert_eq!(aa, bb);
    }

    #[test]
    fn wrap_native_test() {
        let sym = next_symbol('k', 0);
        let output = cps_translate(t("(+ 1 2)"), sym.clone(), 0).unwrap();

        let expected = t(&format!("(+/k 1 2 {})", sym));

        println!("output = {}", output);
        println!("expected = {}", expected);

        string_equals(&expected, &output);
        // assert_eq!(t("(fn (k0) (k0 (+ 1 2))))"), output)
    }

    #[test]
    fn simple_nested_cps_test() {
        let s = next_symbol('o', 0);
        let output = cps_translate(t("(+ (+ 1 2) 2))"), s.clone(), 0).unwrap();

        let e = t(&format!("(+/k 1 2 (fn (a0) (+/k a0 2 {})))", s));

        string_equals(&e, &output);
    }

    #[test]
    fn trivial_cps_test() {
        let s = next_symbol('k', 0);
        let output = cps_translate(t("(+ 1 2))"), s.clone(), 0).unwrap();

        let e = t("(+/k 1 2 k0)");

        string_equals(&e, &output);
    }

    #[test]
    fn two_things_to_do() {
        let s = next_symbol('s', 0);
        let output = cps_translate(t("(+ (+ 1 2) (+ 3 4) 5)"), s.clone(), 0).unwrap();

        string_equals(&t("(+/k 3 4 (fn (a1) (+/k 1 2 (fn (a0) (+/k a0 a1 5 s0)))))"), &output);
    }
}
