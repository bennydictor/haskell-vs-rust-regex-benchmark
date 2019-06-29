extern crate regex;

use regex::{sym, seq, rep};
use regex::matches;


fn main() {
    let a = sym(|c: char| c == 'a');
    let b = sym(|c: char| c == 'b');
    let mut re = seq(rep(seq(seq(seq(rep(a.clone()), b.clone()), rep(a.clone())), b)), rep(a));

    let n = std::env::args().skip(1).next().unwrap().parse().unwrap();
    println!("{}", matches(&mut re, std::iter::repeat("ab".chars()).take(n).flatten()));
}
