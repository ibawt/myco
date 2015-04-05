extern crate regex;

#[derive(Debug, PartialEq)]
enum Atom {
    Integer(i64),
    Float(f64),
    Symbol(String),
    Nil
}

use regex::Regex;

fn tokenize(line: &str) -> Node {
    let r = Regex::new(r"\s+").unwrap();
    let l = line.replace("(", " ( ").replace(")", " ) ").trim().to_string();
    let iter = r.split(&l);
    read_tokens(&mut iter.peekable())
}

fn atom(token: &str) -> Atom {
    let i = token.parse::<i64>();
     match i {
         Ok(v) => Atom::Integer(v),
         _ => {
             let f = token.parse::<f64>();
             match f {
                 Ok(x) => Atom::Float(x),
                 _ => Atom::Symbol(token.to_string())
             }
         },
     }
}

#[test]
fn atom_test() {
    assert_eq!(Atom::Integer(32), atom("32"));
    assert_eq!(Atom::Symbol("symbol".to_string()), atom("symbol"));
}
#[derive(Debug,PartialEq)]
struct Node {
    atom: Atom,
    children: Vec<Node>
}

fn make_atom_node(s: &str) -> Node {
    return Node{ atom: atom(s), children: vec![]};
}

fn read_tokens(iter: &mut std::iter::Peekable<regex::RegexSplits>) -> Node {
    let mut node = Node{ atom: Atom::Nil, children: vec![] };

    match iter.next() {
        Some("(") => {
            loop {
                match iter.peek() {
                    Some(&")") => break,
                    Some(_) => {},
                    None => panic!("unexpected EOF"),
                }
                node.children.push(read_tokens(iter));
            }
        },
        Some(")") => panic!("mismatched )"),
        Some(x) => {
            node.atom = atom(x);
        },
        None => panic!("unexpected EOF")
    }
    return node;
}

#[test]
fn simple_read_tokens() {
    let x = parse("(+ 1 2)");

    assert_eq!(Atom::Nil, x.atom);
    assert_eq!(3, x.children.len());

    let xx : Vec<Node> = vec![make_atom_node("+"), make_atom_node("1"), make_atom_node("2")];

    for pair in xx.iter().zip(x.children.iter()) {
        assert_eq!(pair.0, pair.1);
    }
}

#[test]
fn nested_read_tokens() {
    let x = parse("(+ 1 (* 2 2))");

    assert_eq!( atom("+"), x.children[0].atom);
    assert_eq!( atom("1"), x.children[1].atom);
    assert_eq!( Atom::Nil, x.children[2].atom);

    assert_eq!( 3, x.children[2].children.len());
    assert_eq!( atom("*"), x.children[2].children[0].atom);
    assert_eq!( atom("2"), x.children[2].children[1].atom);
    assert_eq!( atom("2"), x.children[2].children[2].atom);
}

#[test]
#[should_panic]
fn unmatched_bracket() {
    let x = parse("(+ 1");
}

#[test]
#[should_panic]
fn eof() {
    let x = parse("");
}

fn eval(node: &Node) -> Atom {
    return Atom::Nil;
}

fn parse(line: &str) -> Node {
    tokenize(line)
}

pub fn main() {
    let x = parse("(+ 1 (* 3 2))");

    println!("x has {} many children", x.children.len());
}
