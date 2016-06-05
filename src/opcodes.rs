use std::fmt;
use symbol::InternedStr;
use atom::*;

#[derive (Debug, Clone, PartialEq)]
#[allow(non_camel_case_types)]
pub enum Opcode {
    CONST(Atom), // pushes the atom on the stack
    LOAD(InternedStr), // loads then pushes the value
    DEFINE(InternedStr), // sets the symbol to value on the top of the stack
    POP, // pops one off the stack
    STORE(InternedStr), // sets the value to top of stack
    JUMP_IFNOT(usize), // jumps if the stack is falsy
    JUMP(usize), // jumps to PC
    RETURN, // pops the frame
    APPLY,  //
    CALL(Function, usize), // calls a noncompiled or native function
    DCALL(usize), // calls the function at the top of the stack
    RECUR(usize), // tail recursion
}

#[allow(dead_code)]
pub fn print_instructions(instructions: &[Opcode]) -> String {
    instructions.iter()
        .enumerate()
        .map(|(i, n)| format!("{:2} - {}\n", i, n))
        .collect()
}

impl fmt::Display for Opcode {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Opcode::*;
        match *self {
            CONST(ref a) => write!(fmt, "CONST({})", a),
            LOAD(ref a) => write!(fmt, "LOAD({})", *a),
            DEFINE(ref a) => write!(fmt, "DEFINE({})", *a),
            POP => write!(fmt, "POP"),
            STORE(ref a) => write!(fmt, "STORE({})", *a),
            JUMP_IFNOT(i) => write!(fmt, "JUMP_IFNOT({})", i),
            JUMP(i) => write!(fmt, "JUMP({})", i),
            RETURN => write!(fmt, "RETURN"),
            APPLY => write!(fmt, "APPLY"),
            CALL(ref func, arity) => write!(fmt, "CALL({}, {})", func, arity),
            DCALL(arity) => write!(fmt, "DCALL({})", arity),
            RECUR(arity) => write!(fmt, "RECUR({})", arity),
        }
    }
}
