use atom::*;
use number::*;
use env::Env;

#[derive (Debug)]
struct Function2 {
    program: Vec<Instruction>,
    arity: u8
}

#[derive (Debug)]
struct Frame {
    program: Function2,
    sp: usize,
    env: Env,
    caller: usize
}

#[derive (Debug)]
pub struct VirtualMachine {
    program: Vec<Instruction>,
    stack: Vec<Atom>,
    frames: Vec<Frame>,
    env: Env,
    pc: usize,
    fp: usize,
    sp: usize,
}

struct Compiler {
    output: Vec<Instruction>
}

use symbol::*;

impl Compiler {
    fn compile_node(&mut self, a: &Atom) {
        match *a {
            Atom::Symbol(ref sym) => {
                self.push(LOAD_SYM(*sym));
                self.push(LOAD_ENV);
            },
            Atom::List(ref l) => {
                // derp
            },
            Atom::Boolean(b) => {
                self.push(LOADI(b as i64));
            },
            Atom::Number(f) => {
                self.push(LOAD_NUM(f))
            }
        }
    }

    fn compile(&mut self, a: &Atom) {
        match *a {
            Atom::List(ref list) => {
                if list.is_empty() {
                }
            },
            _ => return self.compile_node(a)
        }

        let list = a.as_list().unwrap();

        match list[0] {
            Atom::Form(f) => {
                match f {
                    Form::If => {
                        // TODO: output code for test in list[1]
                        let start_pos = self.compile( list[1] );

                        let jump_pos = self.push(JUMP_IFN(0));
                        self.compile(&list[2]);

                        let true_clause_jump = self.push(JUMP(0));

                        let else_pos = self.compile(list[3]);
                        self.set_jump_pos(jump_pos, else_pos);

                        self.set_jump_pos(JUMP(0), self.cur_pos())
                    }
                }
            }
        }
    }


    pub fn compile_procedure(p: &Procedure) -> Function2 {
        
    }

}

#[derive (Debug,Clone, Copy, PartialEq)]
enum Instruction {
    // control
    NOP,
    DUP,
    POP,
    CALL,
    TCALL,
    JUMP_IF(i32),
    JUMP(usize),
    JUMP_REL(i64),
    RET,

    //lispyness
    CONS,
    LIST,
    APPLY,

    // equivalence
    EQ,
    EQV,
    EQUAL,
    NOT,

    // predicates
    NULLP,
    BOOLEANP,
    SYMBOLP,
    NUMBERP,
    LISTP,
    KEYWORDP,
    FUNCTIONP,
    MACROP,

    // math
    ADD, SUB, MUL, DIV, IDIV, NUMEQ, LT, COMPARE,

    LOADI(i64),
    LOAD_ENV,
    LOAD_SYM(symbol::InternedStr),
    PRINT,
}
use self::Instruction::*;

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            program: vec![],
            stack: vec![],
            frames: vec![],
            env: Env::new(None),
            pc: 0,
            sp: 0,
            fp: 0
        }
    }

    pub fn run(&mut self) -> AtomResult {
        loop {
            if self.pc >= self.program.len() {
                break;
            }
            match self.program[self.pc] {
                ADD => {
                    let a = self.pop().as_number().unwrap();
                    let b = self.pop().as_number().unwrap();
                    self.push( Atom::Number(a + b));
                    self.pc += 1;
                },
                LOADI(i) => {
                    self.push( Atom::Number(Number::Integer(i)));
                    self.pc += 1;
                },
                PRINT => {
                    println!("{}", self.pop());
                    self.pc += 1;
                },
                JUMP_REL(offset) => {
                    self.pc = ((self.pc as i64) + offset) as usize;
                    assert!( self.pc < self.program.len())
                },
                JUMP(addr) => {
                    self.pc = addr;
                    assert!( self.pc < self.program.len())
                },
                POP => {
                    self.pop();
                },
                LOAD_ENV => {
                    let atom = self.pop();
                    let key = try!(atom.as_symbol());
                    let v = self.env.get(key);
                    self.push(v.unwrap_or(Atom::Nil));
                    self.pc += 1;
                },
                CALL => {
                    let func = self.pop();
                    match try!(func.as_function()) {
                        &Function::Proc(ref p) => {
                            // let e = Env::new(Some(p.closures.clone()))
                            //     .bind(&p.params, );
                            // for _ in 1..p.len() {
                            //     self.pop();
                            //     self.sp -= 1;
                            // }
                        }
                        _ => ()
                    }
                }
                _ => ()
            }
        }
        Ok(self.pop())
    }

    fn push(&mut self, a: Atom)  {
        self.stack.push(a);
        self.sp += 1
    }

    fn pop(&mut self) -> Atom {
        self.sp -= 1;
        self.stack.pop().unwrap()
    }
}
