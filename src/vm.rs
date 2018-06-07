use atom::*;
use env::Env;
use errors::*;
use funcs::*;
use eval::*;
use opcodes::*;
use opcodes::Opcode::*;
use compiler;
use parser::tokenize;

#[derive (Debug, Clone)]
struct Frame {
    program: CompiledFunction,
    pc: usize,
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

#[derive (Debug)]
pub struct VirtualMachine {
    stack: Vec<Atom>,
    frames: Vec<Frame>,
    fp: usize,
    sp: usize,
    root: Env,
}

impl Default for VirtualMachine {
    fn default() -> VirtualMachine {
        use base_lib;
        let mut vm = VirtualMachine {
            stack: vec![],
            frames: vec![],
            fp: 0,
            sp: 0,
            root: Env::default(),
        };
        base_lib::library()
            .and_then(|n| vm.run_node(n))
            .expect("base library should always compile and run!");

        vm
    }
}

fn eval_native_borrow(v: &mut VirtualMachine, n: Native, len: usize) -> AtomResult {
    let &mut VirtualMachine { ref mut stack, ref mut frames, fp, .. } = v;
    eval_native(n, &stack[len..], &mut frames[fp].program.env)
}

fn recur_borrow(v: &mut VirtualMachine, len: usize) {
    let &mut VirtualMachine { ref mut stack, ref mut frames, fp, sp, .. } = v;
    let frame = &mut frames[fp];
    frame.program.env.bind_mut(&frame.program.params, &stack[sp - len..]);
    frame.pc = 0;
}

impl VirtualMachine {
    pub fn new_nostdlib() -> VirtualMachine {
        VirtualMachine{
            stack: vec![],
            frames: vec![],
            fp: 0,
            sp: 0,
            root: Env::default()
        }
    }
    pub fn run_node(&mut self, node: Atom) -> AtomResult {
        let mut out = vec![];
        let source = try!(node.as_list()).clone();
        let mut e = self.root.clone();
        try!(compiler::compile(compiler::cps_translate_program(node)?, &mut out, &mut e));
        // try!(compiler::compile(node, &mut out, &mut e));
        let frame = Frame::new(CompiledFunction {
            body: out,
            source: source,
            params: empty_list(),
            env: e,
        });
        self.frames.push(frame);
        self.run()
    }

    pub fn print_stack(&self) {
        for (i, item) in self.stack.iter().rev().enumerate() {
            println!("{}: {}", -(i as i32), item);
        }
    }

    pub fn eval_string(&mut self, s: &str) -> AtomResult {
        use parser;
        parser::tokenize(s).and_then(|a| self.run_node(a))
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
        while let Some(instruction) = self.next_instruction() {
            println!("{} - {}", self.current_frame().pc, instruction);
            match instruction {
                JUMP_IFNOT(addr) => {
                    if !try!(self.pop()).as_bool() {
                        self.current_frame().pc = addr;
                        continue;
                    }
                }
                EVAL => {
                    let e = try!(self.pop());
                    let eval_string = try!(e.as_string());
                    let tokens = try!(tokenize(eval_string));
                    self.fp += 1;
                    let value = self.run_node(tokens);
                    self.fp -= 1;
                    self.push(try!(value));
                }
                JUMP(addr) => {
                    self.current_frame().pc = addr;
                    continue;
                }
                POP => {
                    try!(self.pop());
                }
                RETURN => {
                    self.frames.pop();
                    if self.fp > 0 {
                        self.fp -= 1;
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
                APPLY => {
                    let args = try!(self.pop());
                    let func = try!(self.pop());
                    let arg_list = try!(args.as_list());
                    match func {
                        Atom::Function(Function::Compiled(mut f)) => {
                            f.env.bind_mut(&f.params, arg_list);
                            self.frames.push(Frame::new(f.clone()));
                            self.fp += 1;
                            continue; // don't advance PC of the new frame
                        }
                        Atom::Function(Function::Native(f)) => {
                            let x = try!(eval_native(f,
                                                     arg_list,
                                                     &mut self.frames[self.fp].program.env));
                            self.push(x);
                        }
                        Atom::Function(Function::Proc(ref f)) => {
                            warn!("compiled functions only!");
                            warn!("{}", print_list(&f.body));
                            return Err(ErrorKind::NotImplemented.into());
                        }
                        _ => {
                            warn!("attempted to call: {}", func);
                            bail!(ErrorKind::NotAFunction);
                        }
                    }
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
                            warn!("compiled functions only!");
                            warn!("{}", print_list(&f.body));
                            bail!(ErrorKind::NotImplemented);
                        }
                        Atom::Function(Function::Native(n)) => {
                            let len = self.stack.len();
                            let r = try!(eval_native_borrow(self, n, len - arity));
                            for _ in 0..arity {
                                try!(self.pop());
                            }
                            self.push(r);
                        }
                        _ => {
                            warn!("attempted to call: {}", func);
                            bail!(ErrorKind::NotAFunction);
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
                        Function::Continuation(nc) => {
                            let len = self.stack.len();
                            let k = try!(self.pop());
                            let r = try!(eval_native_borrow(self, nc, len - arity));
                            let arity = arity - 1;
                            for _ in 0..arity {
                                try!(self.pop());
                            }
                            self.push(r);
                            match k {
                                Atom::Function(Function::Compiled(mut f)) => {
                                    let arity = 1;
                                    self.print_stack();
                                    f.env.bind_mut(&f.params, &self.stack[self.sp - arity..]);
                                    for _ in 0..arity {
                                        try!(self.pop());
                                    }
                                    self.frames.push(Frame::new(f.clone()));
                                    self.fp += 1;
                                    continue; // don't advance PC of the new frame
                                },
                                Atom::Function(Function::Native(native)) => {
                                    let len = self.stack.len();
                                    let arity = 1;
                                    let r = try!(eval_native_borrow(self, native, len - arity));
                                    for _ in 0..arity {
                                        try!(self.pop());
                                    }
                                    self.push(r);
                                },
                                _ => bail!(ErrorKind::NotImplemented)
                            }
                        }
                        _ => bail!(ErrorKind::NotImplemented),
                    }
                }
            }
            self.current_frame().pc += 1;
        }
        self.frames.pop().unwrap();
        self.pop()
    }

    fn push(&mut self, a: Atom) {
        self.stack.push(a);
        self.sp += 1
    }

    fn pop(&mut self) -> AtomResult {
        if self.sp > 0 {
            self.sp -= 1;
            self.stack.pop().ok_or(ErrorKind::RuntimeAssertion.into())
        } else {
            error!("stack = {:?}", &self.stack);
            error!("StackUnderflow!");
            bail!(ErrorKind::RuntimeAssertion)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_expr(s: &str) -> Atom {
        let p = tokenize(s).unwrap();
        let mut vm = VirtualMachine::default();
        vm.run_node(p).unwrap()
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

    // #[test]
    // fn let_binding() {
    //     assert_eq!(Atom::from(-1), run_expr("(let* ((x 2) (y 3)) (- x y))"));
    // }

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
    fn filter_test() {
        assert_eq!(Atom::list(vec![Atom::from(0)]),
                   run_expr("(filter (fn (x) (= 0 x)) '(1 1 1 1 0 1))"));
    }

    #[test]
    fn recur_test() {
        let s = include_str!("../test/recur.myco");
        assert_eq!(Atom::from(0), run_expr(s));
    }

    #[test]
    fn load_test() {
        assert_eq!(Atom::from(0), run_expr("(load \"test/recur.myco\")"));
    }

    #[test]
    fn map_test() {
        assert_eq!(run_expr("'(1 2)"),
                   run_expr("(map (fn (x) (+ x 1)) '(0 1))"));

    }

    #[test]
    fn map_leak_test() {
        let mut vm = VirtualMachine::default();
        let l = Atom::list(vec![Atom::from(1), Atom::from(2)]);
        assert_eq!(l, vm.eval_string("(map (fn (x) (+ x 1)) '(0 1))").unwrap());
        assert_eq!(l, vm.eval_string("(map (fn (x) (+ x 1)) '(0 1))").unwrap());
    }


    #[test]
    fn reduce_test() {
        assert_eq!(Atom::from(0),
                   run_expr("(reduce (fn (acc i) (- acc i)) 5 '(1 1 1 1 1))"));
    }

    // #[test]
    // fn apply_test() {
    //     assert_eq!(Atom::from(0), run_expr("(apply (fn (x) 0) '())"));
    //     assert_eq!(Atom::from(3),
    //                run_expr("(apply (fn (x y) (+ x y 1)) '(1 1))"));
    //     assert_eq!(Atom::from(1),
    //                run_expr("(apply (fn (x & y) (count y)) '(1 1))"));
    // }

    // #[test]
    // fn get_test() {
    //     assert_eq!(Atom::from(0), run_expr("(get '(0 2) 0)"));
    //     assert_eq!(Atom::from(0), run_expr("(get '(2 0) 1)"));
    // }

    #[test]
    fn cond_test() {
        assert_eq!(Atom::from(0), run_expr("(cond (true 0))"));
        assert_eq!(Atom::from(0), run_expr("(cond (false 1) (true 0))"));
        assert_eq!(Atom::from(0),
                   run_expr("(cond (false 1) ((= 0 1) 2) (true 0))"));
    }

    #[test]
    fn map_native_test() {
        assert_eq!(Atom::list(vec![Atom::from(1), Atom::from(2)]),
                   run_expr("(map + '(1 2))"));
    }

    #[test]
    fn eval_test() {
        assert_eq!(Atom::from(0), run_expr("(eval \"0\")"));
    }

    #[test]
    fn cont_test() {
        assert_eq!(Atom::from(3), run_expr("(+/k 1 2 (fn (x) x))"));
    }

    #[test]
    fn run_suite() {
        let suite = include_str!("../test/suite.myco");
        assert_eq!(Atom::from(true), run_expr(suite));
    }
}
