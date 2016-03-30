use atom::*;
use env::Env;
use errors::*;
use symbol::*;
use funcs::*;
use eval::*;
use self::Instruction::*;

#[derive (Debug, Clone)]
pub struct Frame {
    pub program: CompiledFunction,
    pub pc: usize
}

impl Frame {
    fn new(f: CompiledFunction) -> Frame {
        Frame {
            program: f,
            pc: 0
        }
    }

    fn current_instruction(&self) -> Option<&Instruction> {
        self.program.body.get(self.pc)
    }
}

#[derive (Debug)]
pub struct VirtualMachine {
    pub stack: Vec<Atom>,
    pub frames: Vec<Frame>,
    fp: usize,
    sp: usize,
}


#[derive (Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Instruction {
    CONST(Atom), // pushes the atom on the stack
    LOAD(InternedStr), // loads then pushes the value
    DEFINE(InternedStr), // sets the symbol to value on the top of the stack
    POP, // pops one off the stack
    STORE(InternedStr),
    // FUNCTION,
    JUMP_IFNOT(usize),
    JUMP(usize),
    RETURN, // pops the frame
    CALL(Function, usize), // calls a noncompiled or native function
    DCALL(usize) // calls the function at the top of the stack
}

fn compile_node(node: Atom, out: &mut Vec<Instruction>, env: &mut Env) -> Result<(), Error> {
    match node {
        Atom::Symbol(sym) => {
            out.push(Instruction::LOAD(sym))
        },
        Atom::List(ref list) => {
            for n in list.iter() {
                try!(compile(n.clone(), out, env));
            }
        },
        // handle constants
        _ => {
            out.push(Instruction::CONST(node.clone()));
        }
    }
    Ok(())
}

fn expand_quasiquote(node: &Atom, env: &Env) -> AtomResult {
    // println!("expand_quasiquote: {}", node);
    if !node.is_pair() {
        return Ok(Atom::list(vec![Atom::Form(Form::Quote), node.clone()]))
    }

    let list = try!(node.as_list());
    match list[0] {
        Atom::Form(Form::Unquote) => {
            try!(expect_arg_length(list, 2));
            return Ok(list[1].clone())
        },
        _ => ()
    }

    if list[0].is_pair() {
        match list[0] {
            Atom::List(ref sublist) => {
                match sublist[0] {
                    Atom::Form(Form::Splice) => {
                        let append = Atom::Function(Function::Native(Native::Append));
                        let rest = list[1..].iter().map(|n| n.clone()).collect();
                        return Ok(Atom::list(vec![append, sublist[1].clone(),
                                                  Atom::list(rest)]))
                    },
                    _ => ()
                }
            },
            _ => ()
        }
    }

    let rest = list[1..].iter().map(|n| n.clone()).collect();
    Ok(Atom::list(vec![//Atom::Function(Function::Native(Native::Cons)),
                       try!(expand_quasiquote(&list[0], env)),
                       try!(expand_quasiquote(&Atom::list(rest), env))]))
}

fn macro_expand(node: Atom, env: &mut Env) -> Result<Atom, Error> {
    match node {
        Atom::List(ref list) if !list.is_empty() => {
            match list[0] {
                Atom::Symbol(ref sym) => {
                    if let Some(Atom::Function(Function::Macro(ref m))) = env.get(sym.as_ref()) {
                        return macro_expand(try!(eval_macro(m, &list[1..], env)), env)
                    }
                },
                Atom::Form(Form::Fn) => {
                    if list.len() < 3 {
                        return Err(Error::NotEnoughArguments)
                    }

                    let mut out = Vec::with_capacity(list.len());
                    out.push(list[0].clone());
                    out.push(list[1].clone());
                    for i in &list[2..] {
                        out.push(try!(macro_expand(i.clone(), env)));
                    }
                    return Ok(Atom::list(out))
                }
                _ => ()
            }
            return Ok(Atom::list(try!(list.iter().map(|n| macro_expand(n.clone(), env)).collect())))
        },
        _ => ()
    }

    Ok(node)
}

pub fn compile(node: Atom, out: &mut Vec<Instruction>, env: &mut Env) -> Result<(), Error> {
    // println!("compiling: {}", node);
    match node {
        Atom::List(ref list) => {
            if list.is_empty() {
                out.push(Instruction::CONST(node.clone()));
                return Ok(())
            }
            ()
        },
        _ => return compile_node(node, out, env)
    }

    let n = try!(macro_expand(node, env));

    let list = match n {
        Atom::List(ref list) => {
            if list.is_empty() {
                out.push(CONST(Atom::Nil));
                return Ok(())
            } else {
                list
            }
        },
        c @ _ => {
            out.push(CONST(c));
            return Ok(())
        }
    };

    if let Atom::List(_) = list[0] {
        for i in list.iter().skip(1) {
            try!(compile(i.clone(), out, env));
        }
        try!(compile(list[0].clone(), out, env));
        out.push(DCALL(list.len() - 1));
        return Ok(())
    }

    match list[0] {
        Atom::Form(f) => {
            match f {
                Form::If => {
                    match list.len() {
                        4 => {
                            try!(compile(list[1].clone(), out, env));
                            // predicate on stack
                            out.push(JUMP_IFNOT(0));
                            let else_jump_pos = out.len() -1;
                            try!(compile(list[2].clone(), out, env));

                            out.push(JUMP(0));
                            let out_jump = out.len() - 1;

                            out[else_jump_pos] = JUMP_IFNOT(out.len());

                            try!(compile(list[3].clone(), out , env));

                            out[out_jump] = JUMP(out.len());
                        },
                        3 => {
                            try!(compile(list[1].clone(), out, env));
                            // predicate on stack
                            out.push(JUMP_IFNOT(0));
                            let else_jump_pos = out.len() -1;
                            try!(compile(list[2].clone(), out, env));
                            out[else_jump_pos] = JUMP_IFNOT(out.len());
                        }
                        _ => {
                            return Err(invalid_arg("invalid number for forms for if"))
                        }
                    }
                    return Ok(())
                },
                Form::Let => {
                    let mut new_env = Env::new(Some(env.clone()));
                    let bind_list = try!(list[1].as_list());
                    let mut bindings = Vec::new();

                    for binding in bind_list.iter() {
                        let bind_exp = try!(binding.as_list());
                        try!(expect_arg_length(bind_exp, 2));
                        bindings.push(bind_exp[0].clone());
                        try!(compile(bind_exp[1].clone(), out, env));
                    }
                    let mut body = Vec::new();
                    try!(compile(list[2].clone(), &mut body, &mut new_env));
                    body.push(RETURN);

                    let arity = bindings.len();

                    let func = CompiledFunction {
                        body: body,
                        params: to_list(bindings),
                        env: new_env
                    };

                    out.push(CONST(Atom::Function(Function::Compiled(func))));
                    out.push(DCALL(arity));
                    return Ok(())
                },
                Form::Set => {
                    let sym = try!(list[1].as_symbol());
                    try!(compile(list[2].clone(), out, env));
                    out.push(STORE(*sym));
                    return Ok(())
                },
                Form::Def => {
                    let sym = try!(list[1].as_symbol());
                    try!(compile(list[2].clone(), out, env));
                    out.push(DEFINE(*sym));
                    return Ok(())
                },
                Form::Do => {
                    if list.len() > 2 {
                        for i in 1..list.len()-1 {
                            try!(compile(list[i].clone(), out, env));
                            out.push(POP);
                        }
                    }
                    return compile(list[list.len()-1].clone(), out, env)
                },
                Form::Fn => {
                    let mut body = Vec::new();
                    let mut env = Env::new(Some(env.clone()));
                    try!(compile(list[2].clone(), &mut body, &mut env));
                    body.push(RETURN);
                    let func = CompiledFunction {
                        body: body,
                        params: try!(list[1].as_list()).clone(),
                        env: env
                    };
                    out.push(CONST(Atom::Function(Function::Compiled(func))));
                    return Ok(())
                },
                Form::Macro => {
                    try!(macro_form(&list[1..], env));
                    out.push(CONST(Atom::Nil));
                    return Ok(())
                },
                Form::Quote => {
                    out.push(CONST(list[1].clone()));
                    return Ok(())
                },
                Form::QuasiQuote => {
                    let expanded = try!(expand_quasiquote(&list[1], env));
                    // println!("expanded: {}", expanded);
                    try!(compile(expanded, out, env));
                    return Ok(())
                }
                _ => {
                    return Err(Error::NotImplemented)
                }
            }
        },
        Atom::Symbol(_) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            try!(compile_node(list[0].clone(), out, env));
            out.push( DCALL(list.len() -1 ));
            return Ok(())
        }
        Atom::Function(ref func) => {
            for n in list.iter().skip(1) {
                try!(compile(n.clone(), out, env));
            }
            match *func {
                Function::Native(_) => {
                    out.push(Instruction::CALL(func.clone(), list.len() -1));
                    return Ok(())
                },
                Function::Compiled(_) => {
                    out.push(CALL(func.clone(), list.len() -1));
                    return Ok(())
                },
                _ => {
                    return Err(Error::NotImplemented)
                }
            }
        },
        _ => {
            return Err(Error::NotAFunction)
        }
    }

    Err(invalid_arg("bottom here"))
}

fn eval_native_borrow(v: &mut VirtualMachine, n: Native, len: usize) -> AtomResult {
    let &mut VirtualMachine { ref mut stack, ref mut frames, fp, .. } = v;
    eval_native(n, &stack[len..], &mut frames[fp].program.env)
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            stack: vec![],
            frames: vec![],
            sp: 0,
            fp: 0
        }
    }

    pub fn reset(&mut self) {
        self.stack.clear();
        self.frames.clear();
        self.sp = 0;
        self.fp = 0;
    }

    fn next_instruction(&self) -> Option<Instruction> {
        self.frames[self.fp].current_instruction().map(|n| n.clone())
    }

    fn current_frame(&mut self) -> &mut Frame {
        &mut self.frames[self.fp]
    }

    fn current_env(&mut self) -> &mut Env {
        &mut self.current_frame().program.env
    }

    pub fn run(&mut self) -> AtomResult {
        println!("running...");
        while let Some(instruction) = self.next_instruction() {
            match instruction {
                JUMP_IFNOT(addr) => {
                    if !try!(self.pop()).as_bool() {
                        self.current_frame().pc = addr;
                        continue;
                    }
                },
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
                        return self.pop()
                    }
                },
                LOAD(sym) => {
                    let value = self.current_frame().program.env.get(sym.as_ref()).unwrap_or(Atom::Nil);
                    self.push(value);
                },
                CONST(ref atom) => {
                    self.push(atom.clone())
                },
                DEFINE(sym) => {
                    let v = try!(self.pop());
                    let a = try!(self.current_env().define(sym, v));
                    self.push(a);
                },
                STORE(sym) => {
                    let v = try!(self.pop());
                    let a = try!(self.current_env().set(sym, v));
                    self.push(a);
                }
                DCALL(arity) => {
                    let func = try!(self.pop());
                    match func {
                        Atom::Function(Function::Compiled(mut f)) => {
                            f.env.bind_mut(&f.params, &self.stack[self.sp-arity..]);
                            for _ in 0..arity {
                                try!(self.pop());
                            }
                            self.frames.push(Frame::new(f.clone()));
                            self.fp += 1;
                            continue; // don't advance PC of the new frame
                        },
                        _ => {
                            // println!("not a function: {:?}", func);
                            return Err(Error::NotAFunction)
                        }
                    }
                },
                CALL(func, arity) => {
                    match func {
                        Function::Native(n) => {
                            let len = self.stack.len();
                            let r = try!(eval_native_borrow(self, n, len - arity));
                            for _ in 0..arity {
                                try!(self.pop());
                            }
                            self.push(r);
                        },
                        _ => {
                            return Err(Error::NotImplemented)
                        }
                    }
                },
            }
            self.current_frame().pc += 1;
        }
        self.pop()
    }

    fn push(&mut self, a: Atom)  {
        self.stack.push(a);
        self.sp += 1
    }

    fn pop(&mut self) -> AtomResult {
        if self.sp > 0 {
            self.sp -= 1;
            return self.stack.pop().ok_or(Error::RuntimeAssertion)
        } else {
            return Err(Error::RuntimeAssertion)
        }
    }
}

pub fn empty_frame() -> Frame {
    Frame::new(CompiledFunction{
        body: Vec::new(),
        params: empty_list(),
        env: Env::new(None)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::*;
    use atom::*;
    use env::*;

    fn run_expr(s: &str) -> Atom {
        let p = tokenize(s).unwrap();
        let mut out = Vec::new();
        let mut env = Env::new(None);
        compile(p, &mut out, &mut env).unwrap();
        let mut vm = VirtualMachine::new();
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
        assert_eq!(Atom::from(3), run_expr("(do (def foo (fn (x) (+ x 2))) (foo 1))"));
    }

    #[test]
    fn test_invoke_fn_inline() {
        assert_eq!(Atom::from(1), run_expr("((fn (x) 1) 0)"));
    }

    #[test]
    fn test_closure() {
        assert_eq!(Atom::from(2), run_expr("(do (def foo (fn (x) (fn () (* 2 x)))) (def foo2 (foo 1)) (foo2))"))
    }

    #[test]
    fn let_binding() {
        assert_eq!(Atom::from(-1), run_expr("(let* ((x 2) (y 3)) (- x y))"));
    }

    #[test]
    fn nested_let() {
        assert_eq!(Atom::from(0), run_expr("(do (let* ((x 1)) (let* ((x 0)) x)))"))
    }

    #[test]
    fn if_test() {
        assert_eq!(Atom::from(0), run_expr("(if false 1 0)"));
        assert_eq!(Atom::from(1), run_expr("(if true 1 0)"));
    }

    #[test]
    fn quote_test() {
        assert_eq!(Atom::list(vec![Atom::from(0), Atom::from(1)]), run_expr("'(0 1)"));
    }
}
