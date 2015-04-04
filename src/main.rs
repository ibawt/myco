extern crate regex;

#[derive(Debug, PartialEq)]
enum Atom {
    Integer(i32),
    Symbol(String),
    Nil
}

use regex::Regex;

fn tokenize(line: &str) -> String {
    let l = line.replace("(", " ( ").replace(")", " ) ");

    return l;
}

fn atom(token: &str) -> Atom {
    let i = token.parse::<i32>();
     match i {
         Ok(v) => {
             return Atom::Integer(v)
         },
         Err(e) => return Atom::Symbol(token.to_string()),
     }
}

#[test]
fn atom_test() {
    assert_eq!(Atom::Integer(32), atom("32"));
    assert_eq!(Atom::Symbol("symbol".to_string()), atom("symbol"));
}
#[derive(Debug)]
struct Node {
    atom: Atom,
    children: Vec<Node>
}

fn read_tokens(tokens: String) -> Node {
    let r = Regex::new(r"\s+").unwrap();
    let mut iter = r.split(&tokens.trim());
    let mut node = Node{ atom: Atom::Nil, children: vec![] };

    loop {
        match iter.next() {
            Some("(") => {
                node.children = vec![];
                println!("(");
                loop {
                    match iter.next() {
                        Some(")") => break,
                        Some(v) => node.children.push(read_tokens(v.to_string())),
                        None => break,
                    }
                }
            },
            Some(")") => panic!("mismatched )"),
            Some(x) => {
                node.atom = atom(x);
            },
            None => break,
        }
    }
    return node;
}

#[test]
fn test_read_tokens() {
    let x = read_tokens(tokenize("(+ 1 2)"));
    println!("x = {:?}", x);

    assert_eq!(Atom::Nil, x.atom);
}

pub fn main() {
    read_tokens(tokenize("(+ 1 2)"));
    println!("hello world");
}
