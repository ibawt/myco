use std::collections::HashSet;
use std::rc::Rc;

lazy_static! {
    static ref SYMBOLS: HashSet<Rc<String>> = {
        HashSet::new()
    };
}

pub fn contains(n: &str) -> bool {
    SYMBOLS.contains(n)
}


pub fn fetch(n: &str) -> Rc<String> {
    if !SYMBOLS.contains(n) {
        SYMBOLS.insert(n.to_owned());
    }

    &SYMBOLS.get(n).unwrap()
}
