pub trait Semiring {
    const ZERO: Self;
    const ONE: Self;
    fn add(self, other: Self) -> Self;
    fn mul(self, other: Self) -> Self;
}

impl Semiring for bool {
    const ZERO: bool = false;
    const ONE: bool = true;
    fn add(self, other: bool) -> bool { self || other }
    fn mul(self, other: bool) -> bool { self && other }
}


#[derive(Debug,Clone,Copy)]
pub struct Reg<S, R> {
    is_empty: S,
    is_final: S,
    re: R,
}

#[derive(Debug,Clone,Copy)]
pub struct Eps;

#[derive(Debug,Clone,Copy)]
pub struct Sym<F>(F);

#[derive(Debug,Clone,Copy)]
pub struct Alt<S, P, Q>(Reg<S, P>, Reg<S, Q>);

#[derive(Debug,Clone,Copy)]
pub struct Seq<S, P, Q>(Reg<S, P>, Reg<S, Q>);

#[derive(Debug,Clone,Copy)]
pub struct Rep<S, R>(Reg<S, R>);


pub fn eps<S: Semiring>() -> Reg<S, Eps> {
    Reg {
        is_empty: S::ONE,
        is_final: S::ZERO,
        re: Eps,
    }
}

pub fn sym<C, S: Semiring, F: FnMut(C) -> S>(pred: F) -> Reg<S, Sym<F>> {
    Reg {
        is_empty: S::ZERO,
        is_final: S::ZERO,
        re: Sym(pred),
    }
}

pub fn alt<S: Semiring + Clone, P, Q>(p: Reg<S, P>, q: Reg<S, Q>) -> Reg<S, Alt<S, P, Q>> {
    Reg {
        is_empty: p.is_empty.clone().add(q.is_empty.clone()),
        is_final: p.is_final.clone().add(q.is_final.clone()),
        re: Alt(p, q),
    }
}

pub fn seq<S: Semiring + Clone, P, Q>(p: Reg<S, P>, q: Reg<S, Q>) -> Reg<S, Seq<S, P, Q>> {
    Reg {
        is_empty: p.is_empty.clone().mul(q.is_empty.clone()),
        is_final: p.is_final.clone().mul(q.is_empty.clone()).add(q.is_final.clone()),
        re: Seq(p, q),
    }
}

pub fn rep<S: Semiring + Clone, R>(r: Reg<S, R>) -> Reg<S, Rep<S, R>> {
    Reg {
        is_empty: S::ONE,
        is_final: r.is_final.clone(),
        re: Rep(r),
    }
}


pub trait Re<C, S>: Sized {
    fn shift(r: &mut Reg<S, Self>, s: S, c: C);
}

impl<C, S> Re<C, S> for Eps {
    fn shift(_r: &mut Reg<S, Self>, _s: S, _c: C) {}
}

impl<C, S: Semiring, F: FnMut(C) -> S> Re<C, S> for Sym<F> {
    fn shift(r: &mut Reg<S, Self>, s: S, c: C) {
        r.is_final = s.mul(r.re.0(c));
    }
}

impl<C: Clone, S: Semiring + Clone, P: Re<C, S>, Q: Re<C, S>> Re<C, S> for Alt<S, P, Q> {
    fn shift(r: &mut Reg<S, Self>, s: S, c: C) {
        Re::shift(&mut r.re.0, s.clone(), c.clone());
        Re::shift(&mut r.re.1, s, c);
        r.is_empty = r.re.0.is_empty.clone().add(r.re.1.is_empty.clone());
        r.is_final = r.re.0.is_final.clone().add(r.re.1.is_final.clone());
    }
}

impl<C: Clone, S: Semiring + Clone, P: Re<C, S>, Q: Re<C, S>> Re<C, S> for Seq<S, P, Q> {
    fn shift(r: &mut Reg<S, Self>, s: S, c: C) {
        let t = r.re.0.is_empty.clone().mul(s.clone()).add(r.re.0.is_final.clone());
        Re::shift(&mut r.re.0, s, c.clone());
        Re::shift(&mut r.re.1, t, c);
        r.is_empty = r.re.0.is_empty.clone().mul(r.re.1.is_empty.clone());
        r.is_final = r.re.0.is_final.clone().mul(r.re.1.is_empty.clone()).add(r.re.1.is_final.clone());
    }
}

impl<C: Clone, S: Semiring + Clone, R: Re<C, S>> Re<C, S> for Rep<S, R> {
    fn shift(r: &mut Reg<S, Self>, s: S, c: C) {
        let t = r.re.0.is_final.clone();
        Re::shift(&mut r.re.0, s.add(t), c);
        r.is_final = r.re.0.is_final.clone()
    }
}

pub fn matches<C, S: Semiring + Clone, R: Re<C, S>>(r: &mut Reg<S, R>, mut cs: impl Iterator<Item = C>) -> S {
    match cs.next() {
        None => r.is_empty.clone(),
        Some(c) => {
            Re::shift(r, S::ONE, c);
            for c in cs {
                Re::shift(r, S::ZERO, c);
            }
            r.is_final.clone()
        }
    }
}
