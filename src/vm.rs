use atom::*;
use env::Env;
use errors::*;
use funcs::*;
use eval::*;
use opcodes::*;
use opcodes::Opcode::*;

#[derive (Debug, Clone)]
pub struct Frame {
    pub program: CompiledFunction,
    pub pc: usize,
}

impl Frame {
    fn new(f: CompiledFunction) -> Frame {
        Frame {
            program: f,
            pc: 0,
        }
    }

    fn current_instruction(&self) -> Option<&Opcode> {
        self.program.body.get(self.pc)
    }
}

#[derive (Debug, Default)]
pub struct VirtualMachine {
    stack: Vec<Atom>,
    frames: Vec<Frame>,
    fp: usize,
    sp: usize,
}

pub fn default_run_node(node: Atom, env: &mut Env) -> AtomResult {
    let mut vm = VirtualMachine::default();
    let mut out = vec![];
    let mut e = env.clone();
    try!(compile(node, &mut out, &mut e));
    let mut frame = empty_frame();
    frame.program.env = e;
    frame.program.body = out;
    vm.frames.push(frame);
    vm.run()
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

fn eval_native_borrow(v: &mut VirtualMachine, n: Native, len: usize) -> AtomResult {
    let &mut VirtualMachine { ref mut stack, ref mut frames, fp, .. } = v;
    eval_native(n, &stack[len..], &mut frames[fp].program.env)
}

fn recur_borrow(v: &mut VirtualMachine, len: usize) {
    let &mut VirtualMachine { ref mut stack, ref mut frames, fp, sp } = v;
    let frame = &mut frames[fp];
    frame.program.env.bind_mut(&frame.program.params, &stack[sp - len..]);
    frame.pc = 0;
}

impl VirtualMachine {
    pub fn reset(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.sp = 0;
        self.fp = 0;
    }

    fn next_instruction(&self) -> Option<Opcode> {
        self.frames[self.fp].current_instruction().cloned()
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.fp]
    }

    fn current_env(&mut self) -> &mut Env {
        &mut self.current_frame().program.env
    }

    pub fn run(&mut self) -> AtomResult {
        // println!("VirtualMachine::run()");
        while let Some(instruction) = self.next_instruction() {
            // println!("{} - {}", self.current_frame().pc, instruction);
            match instruction {
                JUMP_IFNOT(addr) => {
                    if !try!(self.pop()).as_bool() {
                        self.current_frame().pc = addr;
                        continue;
                    }
                }
                JUMP(addr) => {
                    self.current_frame().pc = addr;
                    continue;
                }
                POP => {
                    try!(self.pop());
                }
                RETURN => {
                    if self.fp > 0 {
                        self.fp -= 1;
                        self.frames.pop();
                    } else {
                        return self.pop();
                    }
                }
                LOAD(sym) => {
                    let value =
                        self.current_frame().program.env.get(sym.as_ref()).unwrap_or(Atom::Nil);
                    self.push(value);
                }
                CONST(ref atom) => self.push(atom.clone()),
                DEFINE(sym) => {
                    let v = try!(self.pop());
                    let a = try!(self.current_env().define(sym, v));
                    self.push(a);
                }
                STORE(sym) => {
                    let v = try!(self.pop());
                    let a = try!(self.current_env().set(sym, v));
                    self.push(a);
                }
                RECUR(arity) => {
                    recur_borrow(self, arity);
                    for _ in 0..arity {
                        try!(self.pop());
                    }
                    continue;
                }
                DCALL(arity) => {
                    let func = try!(self.pop());
                    match func {
                        Atom::Function(Function::Compiled(mut f)) => {
                            f.env.bind_mut(&f.params, &self.stack[self.sp - arity..]);
                            for _ in 0..arity {
                                try!(self.pop());
                            }
                            self.frames.push(Frame::new(f.clone()));
                            self.fp += 1;
                            continue; // don't advance PC of the new frame
                        }
                        Atom::Function(Function::Proc(ref f)) => {
                            println!("compiled functions only!");
                            println!("{}", print_list(&f.body));
                            return Err(Error::NotImplemented);
                        }
                        _ => {
                            println!("attempted to call: {}", func);
                            return Err(Error::NotAFunction);
                        }
                    }
                }
                CALL(func, arity) => {
                    match func {
                        Function::Native(n) => {
                            let len = self.stack.len();
                            let r = try!(eval_native_borrow(self, n, len - arity));
                            for _ in 0..arity {
                                try!(self.pop());
                            }
                            self.push(r);
                        }
                        _ => return Err(Error::NotImplemented),
                    }
                }
            }
            self.current_frame().pc += 1;
        }
        self.pop()
    }

    fn push(&mut self, a: Atom) {
        self.stack.push(a);
        self.sp += 1
    }

    fn pop(&mut self) -> AtomResult {
        if self.sp > 0 {
            self.sp -= 1;
            self.stack.pop().ok_or(Error::RuntimeAssertion)
        } else {
            println!("StackUnderflow!");
            Err(Error::RuntimeAssertion)
        }
    }

    pub fn push_frame(&mut self, f: Frame) {
        self.frames.push(f)
    }
}

pub fn empty_frame() -> Frame {
    Frame::new(CompiledFunction {
        body: Vec::new(),
        source: empty_list(),
        params: empty_list(),
        env: Env::new(None),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use atom::*;
    use env::*;
    use base_lib;

    fn run_expr(s: &str) -> Atom {
        let p = tokenize(s).unwrap();
        let mut out = Vec::new();
        let mut env = Env::new(None);
        base_lib::init(&mut env).unwrap();
        compile(p, &mut out, &mut env).unwrap();
        let mut vm = VirtualMachine::default();
        let mut f = empty_frame();
        f.program.env = env;
        f.program.body = out;
        vm.frames.push(f);
        vm.run().unwrap()
    }

    #[test]
    fn test() {
        assert_eq!(Atom::from(6), run_expr("(+ (+ 2 2) 2)"));
        assert_eq!(Atom::symbol("a"), run_expr("(def a 1)"));
        assert_eq!(Atom::from(3),
                   run_expr("(do (def foo (fn (x) (+ x 2))) (foo 1))"));
    }

    #[test]
    fn test_invoke_fn_inline() {
        assert_eq!(Atom::from(1), run_expr("((fn (x) 1) 0)"));
    }

    #[test]
    fn test_closure() {
        assert_eq!(Atom::from(2),
                   run_expr("(do (def foo (fn (x) (fn () (* 2 x)))) (def foo2 (foo 1)) (foo2))"))
    }

    #[test]
    fn let_binding() {
        assert_eq!(Atom::from(-1), run_expr("(let* ((x 2) (y 3)) (- x y))"));
    }

    #[test]
    fn nested_let() {
        assert_eq!(Atom::from(0),
                   run_expr("(do (let* ((x 1)) (let* ((x 0)) x)))"))
    }

    #[test]
    fn if_test() {
        assert_eq!(Atom::from(0), run_expr("(if false 1 0)"));
        assert_eq!(Atom::from(1), run_expr("(if true 1 0)"));
    }

    #[test]
    fn quote_test() {
        assert_eq!(Atom::list(vec![Atom::from(0), Atom::from(1)]),
                   run_expr("'(0 1)"));
    }

    #[test]
    fn recur_test() {
        let s = include_str!("../test/recur.lisp");
        assert_eq!(Atom::from(0), run_expr(s));
    }

    #[test]
    fn map_test() {
        assert_eq!(run_expr("'(1 2)"),
                   run_expr("(map* (fn (x) (+ x 1)) '(0 1))"));
    }

    #[test]
    fn run_suite() {
        let suite = include_str!("../test/suite.lisp");
        assert_eq!(Atom::from(true), run_expr(suite));
    }
}
