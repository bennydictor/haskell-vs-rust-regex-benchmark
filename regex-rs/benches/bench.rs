extern crate regex;

#[macro_use]
extern crate criterion;

use regex::{sym, seq, rep};
use regex::matches;

use std::time::Duration;
use criterion::Criterion;


fn bench(crit: &mut Criterion) {
    let a = sym(|c: char| c == 'a');
    let b = sym(|c: char| c == 'b');
    let r = seq(rep(seq(seq(seq(rep(a.clone()), b.clone()), rep(a.clone())), b)), rep(a));

    crit.bench_function(  "1_000", move |b| b.iter(|| matches(&mut r.clone(), std::iter::repeat("ab".chars()).take(  1_000).flatten())));
    crit.bench_function( "10_000", move |b| b.iter(|| matches(&mut r.clone(), std::iter::repeat("ab".chars()).take( 10_000).flatten())));
    crit.bench_function("100_000", move |b| b.iter(|| matches(&mut r.clone(), std::iter::repeat("ab".chars()).take(100_000).flatten())));
}

criterion_group!{
    name = benches;
    config = Criterion::default().measurement_time(Duration::from_secs(60));
    targets = bench
}

criterion_main!(benches);
