use cop::fof::OpA;
use cop::{Args, Form};

fn at(name: &str) -> Form<&str, (), ()> {
    Form::Atom(name, Args::new())
}

#[test]
fn order() {
    use num_bigint::BigUint;

    // order((a | b) & c) = c & (a | b)
    let ab = at("a") | at("b");
    let abc = ab.clone() & at("c");
    let cab = at("c") & ab.clone();
    assert_eq!(abc.order(), (cab, BigUint::from(2 as usize)));

    // order(((a | b) & (c | d)) | e | f) =
    // (e | f) | ((a | b) & (c | d))
    // (note the additional parentheses around (e | f)!)
    let ab = at("a") | at("b");
    let cd = at("c") | at("d");
    let abcd = ab & cd;
    let abcdef = Vec::from([abcd.clone(), at("e"), at("f")]);
    let abcdef = Form::BinA(OpA::Disj, abcdef);
    let efabcd = (at("e") | at("f")) | abcd.clone();
    assert_eq!(abcdef.order(), (efabcd, BigUint::from(6 as usize)));
}

#[test]
fn cnf0() {
    // cnf((a | b) | c) = (a | b) | c
    let abc = (at("a") | at("b")) | at("c");
    assert_eq!(abc.clone().cnf(), abc);

    // cnf(a | b | c) = a | (b | c)
    let abc = Vec::from([at("a"), at("b"), at("c")]);
    let abc = Form::BinA(OpA::Disj, abc);
    let cnf = at("a") | (at("b") | at("c"));
    assert_eq!(abc.cnf(), cnf);

    // cnf((a & b) & (c & d)) = (a & b) & (c & d)
    let abcd = (at("a") & at("b")) & (at("c") & at("d"));
    assert_eq!(abcd.clone().cnf(), abcd);

    // cnf((a & b) | (c & d)) = (a | c) & (a | d) & (b | c) & (b | d)
    let abcd = (at("a") & at("b")) | (at("c") & at("d"));
    let acad = (at("a") | at("c")) & (at("a") | at("d"));
    let bcbd = (at("b") | at("c")) & (at("b") | at("d"));
    assert_eq!(abcd.cnf(), acad & bcbd);
}

#[test]
fn cnf1() {
    // cnf((a & b & c) | d) = (a | d) & (b | d) & (c | d)
    let abc = Vec::from([at("a"), at("b"), at("c")]);
    let cnf = abc.iter().cloned().map(|x| x | at("d"));
    let cnf = Form::BinA(OpA::Conj, cnf.collect());
    let abc: Form<_, (), ()> = Form::BinA(OpA::Conj, abc);
    let abcd = abc | at("d");
    assert_eq!(abcd.cnf(), cnf);
}

#[test]
fn cnf2() {
    // cnf(((a & b) & c) | d) = ((a | d) & (b | d)) & (c | d)
    let ab = Vec::from([at("a"), at("b")]);
    let cnf = ab.iter().cloned().map(|x| x | at("d")).collect();
    let abc = Form::BinA(OpA::Conj, ab) & at("c");
    let abcd = abc | at("d");
    let cnf = Form::BinA(OpA::Conj, cnf) & (at("c") | at("d"));
    assert_eq!(abcd.cnf(), cnf);
}
