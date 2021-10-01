use cop::fof::{Cnf, Dnf, Nnf, OpA};
use cop::{Args, Fof, Lit};

fn lit<P>(name: P) -> Lit<P, u8, u8> {
    Lit::new(name, Args::new())
}

fn at<P>(name: P) -> Fof<Lit<P, u8, u8>, u8> {
    Fof::Atom(lit(name))
}

fn nlit<P>(name: P) -> Nnf<Lit<P, u8, u8>> {
    Nnf::Lit(lit(name))
}

fn clit<P>(name: P) -> Cnf<Lit<P, u8, u8>> {
    Cnf::Disj(Dnf::Lit(lit(name)))
}

#[test]
fn bina() {
    let abc1 = [at("a"), at("b"), at("c")];
    let abc2 = at("a") & (at("b") & at("c"));
    assert_eq!(Fof::BinA(OpA::Conj, Vec::from(abc1)), abc2);

    assert_eq!(at("a") & Fof::BinA(OpA::Conj, Vec::new()), at("a"));
}

#[test]
fn order() {
    use num_bigint::BigUint;

    // order((a | b) & c) = c & (a | b)
    let ab = nlit("a") | nlit("b");
    let abc = ab.clone() & nlit("c");
    let cab = nlit("c") & ab.clone();
    assert_eq!(abc.order(), (cab, BigUint::from(2 as usize)));

    // order(((a | b) & (c | d)) | e | f) =
    // (e | f) | ((a | b) & (c | d))
    // (note the additional parentheses around (e | f)!)
    let ab = nlit("a") | nlit("b");
    let cd = nlit("c") | nlit("d");
    let abcd = ab & cd;
    let abcdef = Vec::from([abcd.clone(), nlit("e"), nlit("f")]);
    let abcdef = Nnf::BinA(OpA::Disj, abcdef);
    let efabcd = (nlit("e") | nlit("f")) | abcd.clone();
    assert_eq!(abcdef.order(), (efabcd, BigUint::from(6 as usize)));
}

#[test]
fn cnf0() {
    // cnf((a | b) | c) = (a | b) | c
    let abc = (nlit("a") | nlit("b")) | nlit("c");
    let cnf = (clit("a") | clit("b")) | clit("c");
    assert_eq!(abc.cnf(), cnf);

    // cnf(a | b | c) = a | (b | c)
    let abc = Vec::from([nlit("a"), nlit("b"), nlit("c")]);
    let abc = Nnf::BinA(OpA::Disj, abc);
    let cnf = clit("a") | (clit("b") | clit("c"));
    assert_eq!(abc.cnf(), cnf);

    // cnf((a & b) & (c & d)) = (a & b) & (c & d)
    let nnf = (nlit("a") & nlit("b")) & (nlit("c") & nlit("d"));
    let cnf = (clit("a") & clit("b")) & (clit("c") & clit("d"));
    assert_eq!(nnf.cnf(), cnf);

    // cnf((a & b) | (c & d)) = ((a | c) & (a | d)) & (b | c) & (b | d)
    let abcd = (nlit("a") & nlit("b")) | (nlit("c") & nlit("d"));
    let acad = (clit("a") | clit("c")) & (clit("a") | clit("d"));
    let bcbd = (clit("b") | clit("c")) & (clit("b") | clit("d"));
    assert_eq!(abcd.clone().cnf(), acad & bcbd);

    let abcdef = abcd.clone() | (nlit("e") & nlit("f"));
    let cnf = (abcd.clone() | nlit("e")).cnf() & (abcd | nlit("f")).cnf();
    assert_eq!(abcdef.cnf(), cnf);
}

#[test]
fn cnf1() {
    // cnf((a & b & c) | d) = (a | d) & (b | d) & (c | d)
    let abc = Vec::from(["a", "b", "c"]);
    let cnf = abc.iter().cloned().map(|x| clit(x) | clit("d"));
    let cnf = Cnf::Conj(cnf.collect());
    let abc = Nnf::BinA(OpA::Conj, abc.iter().cloned().map(nlit).collect());
    let abcd = abc | nlit("d");
    assert_eq!(abcd.cnf(), cnf);
}

#[test]
fn cnf2() {
    // cnf(((a & b) & c) | d) = ((a | d) & (b | d)) & (c | d)
    let ab = Vec::from(["a", "b"]);
    let cnf = ab.iter().cloned().map(|x| clit(x) | clit("d")).collect();
    let abc = Nnf::BinA(OpA::Conj, ab.iter().cloned().map(nlit).collect()) & nlit("c");
    let abcd = abc | nlit("d");
    let cnf = Cnf::Conj(cnf) & (clit("c") | clit("d"));
    assert_eq!(abcd.cnf(), cnf);
}

#[test]
fn clause() {
    use cop::lean::Clause;

    let dlit = |name| Dnf::Lit(lit(name));
    // clause((a | b) | (c | d)) = [b, a, c, d]
    let cl1 = Clause::from((dlit("a") | dlit("b")) | (dlit("c") | dlit("d")));
    let cl2 = Vec::from([lit("b"), lit("a"), lit("c"), lit("d")]);
    assert!(cl1.into_iter().eq(cl2.into_iter()));
}
