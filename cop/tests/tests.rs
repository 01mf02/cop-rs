use cop::fof::OpA;
use cop::{Args, Form};

fn at(name: &str) -> Form<&str, (), ()> {
    Form::Atom(name, Args::new())
}

#[test]
fn order() {
    use num_bigint::BigUint;
    let a = Form::Atom("a", Args::new());
    let b = Form::Atom("b", Args::new());
    let c = Form::Atom("c", Args::new());
    let ab: Form<_, (), ()> = a | b;
    let abc = ab.clone() & c.clone();
    let cab = c.clone() & ab.clone();
    assert_eq!(abc.order(), (cab, BigUint::from(2 as usize)));
}

#[test]
fn cnf0() {
    let abc = (at("a") | at("b")) | at("c");
    assert_eq!(abc.clone().cnf(), abc);

    let abcd = (at("a") & at("b")) & (at("c") & at("d"));
    assert_eq!(abcd.clone().cnf(), abcd);

    let abcd = (at("a") & at("b")) | (at("c") & at("d"));
    let acad = (at("a") | at("c")) & (at("a") | at("d"));
    let bcbd = (at("b") | at("c")) & (at("b") | at("d"));
    assert_eq!(abcd.cnf(), acad & bcbd);
}

#[test]
fn cnf1() {
    let abc = Vec::from([at("a"), at("b"), at("c")]);
    let cnf = abc.iter().cloned().map(|x| x | at("d"));
    let cnf = Form::BinA(OpA::Conj, cnf.collect());
    let abc: Form<_, (), ()> = Form::BinA(OpA::Conj, abc);
    let abcd = abc | at("d");

    assert_eq!(abcd.cnf(), cnf);
}

#[test]
fn cnf2() {
    let ab = Vec::from([at("a"), at("b")]);
    let cnf = ab.iter().cloned().map(|x| x | at("d")).collect();
    let abc = Form::BinA(OpA::Conj, ab) & at("c");
    let abcd = abc | at("d");
    let cnf = Form::BinA(OpA::Conj, cnf) & (at("c") | at("d"));

    assert_eq!(abcd.cnf(), cnf);
}
