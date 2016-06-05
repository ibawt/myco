use atom::*;
use env::Env;
use errors::*;
use funcs::*;
use eval::*;
use opcodes::*;
use opcodes::Opcode::*;
use compiler;

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
    try!(compiler::compile(node, &mut out, &mut e));
    let mut frame = empty_frame();
    frame.program.env = e;
    frame.program.body = out;
    vm.frames.push(frame);
    vm.run()
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
    use compiler;

    fn run_expr(s: &str) -> Atom {
        let p = tokenize(s).unwrap();
        let mut out = Vec::new();
        let mut env = Env::new(None);
        base_lib::init(&mut env).unwrap();
        compiler::compile(p, &mut out, &mut env).unwrap();
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
