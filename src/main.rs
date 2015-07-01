extern crate readline;

mod errors;
mod number;
mod atom;
mod parser;

use parser::*;

fn repl() {
    println!("Rust Lisp!");
    let mut env = default_env();

    loop {
        match readline::readline(">") {
            Some(s) => {
                if let Ok(p) = parser::tokenize(&s) {
                    match eval(&p, &mut env) {
                        Ok(r) => println!("{}", r),
                        Err(e) => println!("Error in evaluation: {:?}", e)
                    }
                } else {
                    println!("Error in parsing");
                }
            },
            None => {
                println!("Exiting...");
                break;
            }
        }
    }
}

pub fn main() {
    repl();
}

#[cfg(test)]
mod test {
    use parser::*;
    use super::default_env;
    use super::eval;
    use atom::Atom;
    use super::Eval;
    use super::Env;
    use super::EvalResult;

    fn teval(s: &str) -> Eval {
        eval(&tokenize(s).unwrap(), &mut default_env()).unwrap()
    }

    fn teval_env(s: &str, env: &mut Env) -> EvalResult {
        eval(&tokenize(s).unwrap(), env)
    }

    fn make_atom_node(s: &str) -> SyntaxNode {
        return SyntaxNode::Node(Node::Atom(Atom::parse(s)));
    }

    fn as_list(n: &SyntaxNode) -> &Vec<SyntaxNode> {
        match n {
            &SyntaxNode::Node(Node::List(ref l)) => l,
            _ => panic!("don't get here!")
        }
    }

    fn as_atom(n: &SyntaxNode) -> &Atom {
        match n {
            &SyntaxNode::Node(Node::Atom(ref a)) => a,
            _ => panic!("not here!")
        }
    }

    fn atom(s: &str) -> Atom {
        Atom::parse(s)
    }

    #[test]
    fn simple_read_tokens() {
        let x = tokenize("(+ 1 2)").unwrap();

        let l = as_list(&x);

        assert_eq!(3, l.len());

        let xx : Vec<SyntaxNode> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

        for pair in xx.iter().zip(l.iter()) {
            assert_eq!(pair.0, pair.1);
        }
    }

    #[test]
    fn nested_read_tokens() {
        let x = tokenize("(+ 1 (* 2 2))").unwrap();

        let l = as_list(&x);

        assert_eq!( atom("+"), *as_atom(&l[0]));
        assert_eq!( atom("1"), *as_atom(&l[1]));

        let l2 = as_list(&l[2]);

        assert_eq!( 3, l.len());
        assert_eq!( atom("*"), *as_atom(&l2[0]));
        assert_eq!( atom("2"), *as_atom(&l2[1]));
        assert_eq!( atom("2"), *as_atom(&l2[2]));
    }

    #[test]
    fn subexp_token_test() {
        let x = tokenize("(+ 1 (+ 2 3) 4)").unwrap();

        let l = as_list(&x);

        assert_eq!( atom("+"), *as_atom(&l[0]));
        assert_eq!( atom("1"), *as_atom(&l[1]));

        let l2 = as_list(&l[2]);

        assert_eq!( 3, l2.len());
        assert_eq!( atom("+"), *as_atom(&l2[0]));
        assert_eq!( atom("2"), *as_atom(&l2[1]));
        assert_eq!( atom("3"), *as_atom(&l2[2]));

        assert_eq!(atom("4"), *as_atom(&l[3]));
    }

    #[test]
    fn if_special_form() {
        let x = eval(&tokenize("(if (= 1 1) true false)").unwrap(), &mut default_env()).unwrap();

        assert_eq!(Eval::Atom(Atom::Boolean(true)), x);
    }

    #[test]
    fn if_special_form_false() {
        let x = eval(&tokenize("(if (= 1 2) true false)").unwrap(), &mut default_env()).unwrap();

        assert_eq!(Eval::Atom(Atom::Boolean(false)), x);
    }

    #[test]
    fn if_no_else() {
        let x = teval("(if (= 1 1) true)");

        assert_eq!(Eval::Atom(Atom::Boolean(true)), x);
    }

    fn tassert(v: &str) {
        assert_eq!(Eval::Atom(Atom::Boolean(true)), teval(v));
    }

    fn trefute(v: &str) {
        assert_eq!(Eval::Atom(Atom::Boolean(false)), teval(v));
    }

    #[test]
    fn comparisons() {
        tassert("(= 1 1 1 1)");
        trefute("(= 1 0 1 1)");
        tassert("(< 1 5 10)");
        trefute("(< 5 1 20)");
        tassert("(<= 1 1 1 5)");
        trefute("(<= 5 2 1 5)");
        tassert("(> 5 3 2 1)");
        trefute("(> 5 3 2 10)");
        tassert("(>= 5 5 5 3)");
        trefute("(>= 5 5 5 10)");
    }
    use super::number::Number;

    fn num(i: i64) -> Eval {
        Eval::Atom(Atom::Number(Number::Integer(i)))
    }

    #[test]
    fn adds() {
        assert_eq!(num(5), teval("(+ 2 3)"));
        assert_eq!(num(25), teval("(+ 5 5 5 5 5)"));
    }

    #[test]
    fn subs() {
        assert_eq!(num(0), teval("(- 5 5)"));
        assert_eq!(num(5), teval("(- 20 10 5)"));
    }

    #[test]
    fn muls() {
        assert_eq!(num(0), teval("(* 5 5 5 0)"));
        assert_eq!(num(5), teval("(* 1 5)"));
        assert_eq!(num(-5), teval("(* -1 5)"));
    }

    #[test]
    fn symbol_resolving() {
        let mut env = default_env();

        teval_env("(def foo 5)", &mut env).unwrap();

        assert_eq!(num(0), teval_env("(- foo 5)", &mut env).unwrap());
    }

    #[test]
    fn first_and_rest() {
        assert_eq!(num(0), teval("(first (list 0 1 2))"));
        assert_eq!(teval("(list 1 2)"), teval("(rest (list 0 1 2))"));
    }

    #[test]
    fn simple_func() {
        let mut env = default_env();
        let _ = eval(&tokenize("(def 'f (fn (r b) (+ r b)))").unwrap(), &mut env).unwrap();
        let res = eval(&tokenize("(f 2 3)").unwrap(), &mut env).unwrap();

        assert_eq!(num(5), res);
    }

    #[test]
    fn quoting() {
        //assert_eq!("(a (+ 1 2) c)", "'(a (+ 1 2) c)"); 
    }

    #[test]
    #[should_panic]
    fn unmatched_bracket() {
        tokenize("(+ 1").unwrap();
    }

    #[test]
    #[should_panic]
    fn eof() {
         tokenize("").unwrap();
    }
}
