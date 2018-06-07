use atom::*;
use eval::*;
use env::*;
use errors::*;
use opcodes::*;
use opcodes::Opcode::*;


fn next_symbol(prefix: char, n: u32) -> Atom {
    let s = format!("__{}{}", prefix, n);
    Atom::symbol(&s)
}

fn split_args(n: &[Atom]) -> (Vec<Atom>, Vec<Atom>) {
    let mut fns = vec![];
    let mut literals = vec![];

    for i in n {
        match *i {
            Atom::List(_) => {
                fns.push(i.clone());
            }
            _ => {
                literals.push(i.clone());
            }
        }
    }
    (fns, literals)
}

pub fn cps_translate_program(node: Atom) -> Result<Atom> {
    let a = cps_translate(node, Atom::Function(Function::Native(Native::Identity)), 1)?;
    println!("CPS Translate Program: {}", a);
    Ok(a)
}

// Example
// Input: (+ (+ 1 2) 1)
// Output: (+/k 1 2 (fn (a0) (+/k 1 a0 k)))

// Input: (+ 1 2)
// Output: (+/k 1 2 k)

// Input: (+ (+ 1 2) (+ 3 4) 5)
// Output: (+/k 1 2 (fn (a0) (+/k 3 4 (fn (a1) (+/k a0 a1 5 k)))))
pub fn cps_translate(node: Atom, cont: Atom, symbol_count: u32) -> Result<Atom> {
    println!("cps_translate: {}", node);
    println!("continuation: {}, symbol_count: {}", cont, symbol_count);
    if let Atom::List(ref list) = node {
        if list.is_empty() {
            return Ok(Atom::empty_list());
        }
        match list[0] {
            Atom::Function(Function::Native(n)) => {
                let native = Atom::Function(Function::Continuation(n));

                let (fns, literals) = split_args(&list[1..]);

                if fns.is_empty() {
                    let mut x = Vec::with_capacity(literals.len() + 2);
                    x.push(native);
                    for i in literals {
                        x.push(i);
                    }
                    x.push(cont);
                    Ok(Atom::list(x))
                } else {
                    // Wrap myself
                    let args: Vec<Atom> = fns.iter()
                        .enumerate()
                        .map(|(i, _)| next_symbol('a', symbol_count + i as u32))
                        .collect();

                    let mut body = Vec::with_capacity(1 + args.len() + literals.len() + 1);
                    body.push(native);
                    for a in &args {
                        body.push(a.clone());
                    }

                    for i in literals.into_iter() {
                        body.push(i);
                    }

                    body.push(cont);

                    let mut current = Atom::list(vec![Atom::Form(Form::Fn),
                                                      Atom::list(vec![args[0].clone()]),
                                                      Atom::list(body)]);


                    for (i, func) in fns.into_iter().enumerate() {
                        current = cps_translate(func, current, symbol_count + i as u32)?;

                        if i < (args.len() - 1) {
                            current = Atom::list(vec![Atom::Form(Form::Fn),
                                                      Atom::list(vec![args[i + 1].clone()]),
                                                      current]);
                        }
                    }

                    Ok(current)
                }
            }
            Atom::Form(Form::If) => {
                // (if (< 3 (+ 3 4)) (* 5 5) (* 6 6))
                // (+/k 3 4 (lambda (a0) (< 3 a0 (lambda (a1)
                //                                  (if a1
                //                                        (*/k 5 5 k)
                //                                          (*/k 6 6 k)))))
                //
                if let Atom::List(_) = list[1] {
                    let pred_sym = next_symbol('i', symbol_count as u32);
                    let x = Atom::list(vec![Atom::Form(Form::Fn),
                                            Atom::list(vec![pred_sym.clone()]),
                                            Atom::list(vec![Atom::Form(Form::If),
                                                            pred_sym,
                                                            cps_translate(list[2].clone(),
                                                                          cont.clone(),
                                                                          symbol_count + 1)?,
                                                            cps_translate(list[3].clone(),
                                                                          cont,
                                                                          symbol_count + 1)?])]);
                    let pred = cps_translate(list[1].clone(), x, symbol_count + 1)?;

                    return Ok(pred);
                } else {
                    Ok(Atom::list(vec![Atom::Form(Form::If),
                                       list[1].clone(),
                                       cps_translate(list[2].clone(),
                                                     cont.clone(),
                                                     symbol_count + 1)?,
                                       cps_translate(list[3].clone(), cont, symbol_count + 1)?]))

                }
            }
            Atom::Form(Form::Do) => {
                let mut body = vec![Atom::Form(Form::Do)];

                if list.len() < 2 {
                    return Ok(Atom::list(body));
                }

                for i in 1..(list.len() - 1) {
                    let sym = next_symbol('d', symbol_count + i as u32);
                    let mut current =
                        Atom::list(vec![Atom::Form(Form::Fn), Atom::list(vec![sym.clone()]), sym]);

                    current = cps_translate(list[i].clone(), current, symbol_count + i as u32)?;

                    body.push(current);
                }


                let x = cps_translate(list[(list.len() - 1)].clone(),
                                      cont,
                                      symbol_count + (list.len() as u32))?;

                body.push(x);

                Ok(Atom::list(body))
            }
            Atom::Form(Form::Let) => {
                // (let ((x (+ 1 2)) (y (+ 3 4))) (+ x y))
                //  (+/k 3 4
                //     (fn (y)
                //       (+/k 1 2
                //         (fn (x)
                //           (+/k x y k))))

                // (let ((x 1)) x)
                // ((fn (x) x) 1)

                // (let ((x (+ 1 2)) (y (+ 3 4))) (+ x y))
                // ((fn (x y) (+ x y)) (+ 1 2) (+ 3 4))


                // (let* ((x 1)) x)
                // (k ((fn (x) x) 1))
                let binding_pairs = list[1].as_list()?;
                if binding_pairs.is_empty() {
                    bail!(ErrorKind::RuntimeAssertion);
                }
                let mut bindings = vec![];
                let mut bodies = vec![];
                for i in binding_pairs.iter() {
                    let list = i.as_list()?;

                    let x = list.first().ok_or(ErrorKind::RuntimeAssertion)?;
                    println!("bindings = {}", x);

                    bindings.push(x.clone());
                    bodies.push(list.get(1).ok_or(ErrorKind::RuntimeAssertion)?.clone());
                }

                let current = Atom::list(vec![Atom::Form(Form::Fn),
                                              Atom::list(bindings),
                                              list[2].clone(),
                ]);
                println!("currnet: {}", current);
                let mut x = vec![current];
                for i in bodies {
                    x.push(i.clone());
                }
                let current = Atom::list(x);
                return cps_translate(current, cont, symbol_count);
                // // binds = [ x ]
                // // convert the body of the let
                // let mut current =
                //     Atom::list(vec![Atom::Form(Form::Fn),
                //                     Atom::list(vec![bindings[0].clone()]),
                //                     cps_translate(list[2].clone(), cont.clone(), symbol_count)?]);
                // // should return x
                // println!("pre-trans: {}", list[2]);
                // println!("current:  {}", current);

                // let binding_body = cps_translate(binding_pairs[0].as_list()?[1].clone(), current.clone(), symbol_count)?;

                // if list[1].as_list()?.len() == 1 {
                //     println!("list[1].len() == 1");
                //     // current = Atom::list(vec![cont, Atom::list(vec![current.clone(),
                //     //                                                 binding_body,
                //     //                                                 ])]);

                //     current = Atom::list(vec![cont, Atom::list(vec![current.clone(),
                //                                                     binding_body,
                //     ])]);
                //     println!("current: {}", current);

                //     return Ok(current);
                // }

                // current = binding_body;

                // for (i, node) in list[1].as_list()?.iter().enumerate().skip(1) {

                //     let wrap = Atom::list(vec![Atom::Form(Form::Fn),
                //                                Atom::list(vec![bindings[i].clone()]),
                //                                current]);
                //     println!("{}: wrap = {}", i, wrap);

                //     current =
                //         cps_translate(node.as_list()?[i].clone(), wrap, symbol_count + i as u32)?;

                //     println!("{}: current = {}", i, current);

                //     if i < (bindings.len() -1) {
                //         current = Atom::list(vec![Atom::Form(Form::Fn),
                //                                   Atom::list(vec![bindings[i].clone()]),
                //                                   current]);

                //         println!("doing last thingy: {}", current)
                //     }
                // }
                // Ok(current)
            }
            Atom::Form(Form::Def) => {
                let v = cps_translate(list[2].clone(), Atom::identity(), symbol_count)?;
                Ok(Atom::list(vec![Atom::Form(Form::Def),
                                   list[1].clone(),
                                   v]))
            }
            Atom::Form(Form::Fn) => {
                let v = cps_translate(list[2].clone(), Atom::identity(), symbol_count)?;
                Ok(Atom::list(vec![list[0].clone(), list[1].clone(), v]))
            }
            // Atom::Symbol(s) => {
            // }
            _ => {
                Ok(node.clone())
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

fn macro_expand(node: Atom, env: &mut Env) -> Result<Atom> {
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
                        bail!(ErrorKind::NotEnoughArguments(3, list.len()));
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

fn compile_node(node: Atom, out: &mut Vec<Opcode>, env: &mut Env) -> Result<()> {
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

fn compile_form(form: Form, list: &List, out: &mut Vec<Opcode>, env: &mut Env) -> Result<()> {
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
                _ => bail!("invalid number for forms for if"),
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
        _ => bail!(ErrorKind::NotImplemented),
    }
    Ok(())
}

pub fn compile(node: Atom, out: &mut Vec<Opcode>, env: &mut Env) -> Result<()> {
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
                }
                Function::Continuation(_) => {
                    out.push(Opcode::CALL(func.clone(), list.len() - 1));
                    return Ok(());
                }
                Function::Compiled(_) => {
                    out.push(CALL(func.clone(), list.len() - 1));
                    return Ok(());
                }
                _ => bail!(ErrorKind::NotImplemented),
            }
        }
        _ => bail!(ErrorKind::NotAFunction),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser;

    use super::next_symbol;

    fn t(s: &str) -> Atom {
        parser::tokenize_single(s).unwrap()
    }

    fn string_equals(a: &Atom, b: &Atom) {
        let aa = format!("{}", a);
        let bb = format!("{}", b);
        if aa != bb {
            println!("EXPECTED = {}", aa);
            println!("ACTUAL   = {}", bb);
            assert_eq!(aa, bb);
        }
    }

    #[test]
    fn wrap_native_test() {
        let sym = next_symbol('k', 0);
        let output = cps_translate(t("(+ 1 2)"), sym.clone(), 0).unwrap();

        let expected = t(&format!("(+/k 1 2 {})", sym));

        string_equals(&expected, &output);
    }

    #[test]
    fn simple_nested_cps_test() {
        let s = next_symbol('o', 0);
        let output = cps_translate(t("(+ (+ 1 2) 2))"), s.clone(), 0).unwrap();

        let e = t(&format!("(+/k 1 2 (fn (__a0) (+/k __a0 2 {})))", s));

        string_equals(&e, &output);
    }

    #[test]
    fn trivial_cps_test() {
        let s = next_symbol('k', 0);
        let output = cps_translate(t("(+ 1 2))"), s.clone(), 0).unwrap();


        let e = t("(+/k 1 2 __k0)");

        string_equals(&e, &output);
    }

    #[test]
    fn nil_input_cps_test() {
        let s = next_symbol('k', 0);
        let output = cps_translate(Atom::empty_list(), s.clone(), 0).unwrap();

        assert_eq!(Atom::empty_list(), output);
    }

    #[test]
    fn two_things_to_do() {
        let s = next_symbol('s', 0);
        let output = cps_translate(t("(+ (+ 1 2) (+ 3 4) 5)"), s.clone(), 0).unwrap();

        string_equals(&t("(+/k 3 4 (fn (__a1) (+/k 1 2 (fn (__a0) (+/k __a0 __a1 5 __s0)))))"),
                      &output);
    }

    use vm::*;

    fn meval(n: Atom) -> Atom {
        let mut vm = VirtualMachine::new_nostdlib();
        vm.run_node(n).unwrap()
    }

    #[test]
    fn top_level_program_cps() {
        let x = meval(cps_translate_program(t("(+ (+ 1 2) (+ 3 4) 5)")).unwrap());

        assert_eq!(Atom::from(15), x);
    }

    #[test]
    fn do_cps_test() {
        let x = meval(cps_translate_program(t("(do (* 1 2) (+ 3 4))")).unwrap());
        assert_eq!(Atom::from(7), x);
    }

    #[test]
    fn if_cps_test() {
        let x = meval(cps_translate_program(t("(if false (* 1 2) (+ 3 4))")).unwrap());
        assert_eq!(Atom::from(7), x);
    }

    #[test]
    fn more_complex_pred_if_cps_test() {
        let x = meval(cps_translate_program(t("(if (< 4 (+ 5 6)) (* 1 2) (+ 3 4))")).unwrap());
        assert_eq!(Atom::from(7), x);
    }

    #[test]
    fn def_cps_test() {
        let x = meval(cps_translate_program(t("(do (def x 5) (+ 3 4) x)")).unwrap());
        assert_eq!(Atom::from(5), x);
    }

    #[test]
    fn let_cps_test() {
        let x = meval(cps_translate_program(t("(let* ((x (+ 1 2)) (y (+ 3 4))) (+ x y))"))
                          .unwrap());
        assert_eq!(Atom::from(10), x);
    }

    #[test]
    fn let_cps_test_2() {
        let x = meval(cps_translate_program(t("(let* ((x 1) (y 1)) (- y x))"))
                      .unwrap());
        assert_eq!(Atom::from(0), x);
    }

    #[test]
    fn let_literal_test() {
        let x = meval(t("(let* ((x 1)) x)"));
        assert_eq!(Atom::from(1), x);
    }

    #[test]
    fn let_nested_test() {
        let x = meval(cps_translate_program(t("(let* ((x 1)) (let* ((y 1)) (+ x y)))")).unwrap());
        assert_eq!(Atom::from(2), x);
    }
}
