use cop::fof::{Cnf, Fof, FofAtom};
use cop::lean::Matrix;
use cop::szs;
use cop::tptp::SFof;

pub fn add_eq_axioms(fm: SFof) -> Result<SFof, szs::NoSuccessKind> {
    use cop::nonfunctional;
    let (preds, consts) = fm.predconst_unique();
    // check that all symbols occur with the same arities
    if let Some(s) = nonfunctional(&preds).chain(nonfunctional(&consts)).next() {
        log::error!("Arity mismatch: {}", s);
        return Err(szs::SyntaxError);
    }
    let eq = fm.atoms().any(|a| matches!(a, FofAtom::EqTm(_, _)));
    let eq = eq.then(|| {
        Fof::eq_axioms(preds, consts)
            .map_atoms(&mut |a| a.map_vars(&mut |v| v.to_string()))
            .map_vars(&mut |v| v.to_string())
    });
    Ok(eq.into_iter().fold(fm, |fm, eq| fm.add_premise(eq)))
}

pub type SLit<'a> = cop::Lit<cop::Signed<cop::Symbol<'a>>, cop::Symbol<'a>, usize>;

pub fn matrix(fm: Cnf<SLit>) -> Matrix<SLit> {
    let matrix = Matrix::from(fm);

    matrix
        .into_iter()
        .filter(|cl| !cl.is_trivial())
        .map(|cl| {
            let mut map = Default::default();
            let mut st = 0;
            cl.into_iter()
                .map(|lit| lit.fresh_vars(&mut map, &mut st))
                .collect()
        })
        .collect()
}

pub fn hash_matrix<'a>(matrix: &mut Matrix<SLit<'a>>, hash: &SLit<'a>) {
    // insert "#" at the beginning of every positive clause
    for cl in matrix.iter_mut() {
        if cl.iter().all(|lit| lit.head().is_sign_negative()) {
            cl.insert(0, -hash.clone())
        }
    }
}
