use clap::Clap;
use colosseum::unsync::Arena;
use cop::fof::{Cnf, Fof, FofAtom, SkolemState};
use cop::lean::Matrix;
use cop::szs;
use cop::term::Args;
use cop::tptp::SFof;
use cop::{Lit, Signed};
use log::info;

#[derive(Clap)]
pub struct Options {
    /// Disable matrix sorting by number of paths
    #[clap(long)]
    nopaths: bool,
}

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

// "#" marks clauses stemming from the conjecture
// we can interpret it as "$true"
pub fn hash_fof(fm: SFof) -> (bool, SFof) {
    let hash = Fof::atom("#".to_string(), Args::new());
    fm.mark_impl(|| hash.clone())
}

type SLit<'a> = cop::Lit<cop::Signed<cop::Symbol<'a>>, cop::Symbol<'a>, usize>;

fn matrix(fm: Cnf<SLit>) -> Matrix<SLit> {
    let matrix = Matrix::from(fm);
    matrix
        .into_iter()
        .filter(|cl| !cl.is_trivial())
        .map(|cl| cl.fresh_vars(&mut Default::default(), &mut 0))
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

pub fn preprocess<'a>(
    fm: SFof,
    opts: &Options,
    arena: &'a Arena<String>,
) -> (Matrix<SLit<'a>>, SLit<'a>) {
    let fm = fm.map_atoms(&mut |a| a.to_lit(|| "=".to_string()).map_head(Signed::from));
    let fm = fm.qnnf(&Fof::unfold_eqfm_disj_conj);
    info!("unfolded: {}", fm);
    let fm = -fm;
    info!("nnf: {}", fm);
    let fm = fm.fresh_vars(&mut Default::default(), &mut 0);
    info!("fresh vars: {}", fm);
    let fm = fm.skolem_outer(&mut SkolemState::new(("skolem".to_string(), 0)));
    info!("skolemised: {}", fm);
    let fm = if opts.nopaths { fm } else { fm.order().0 };
    info!("ordered: {}", fm);

    let mut set = Default::default();
    let fm = fm.map_literals(&mut |l| l.symbolise(&mut set, arena));
    let hash = Lit::new(Signed::from("#".to_string()), Args::new()).symbolise(&mut set, arena);

    let fm = fm.cnf();
    info!("cnf: {}", fm);

    let matrix = matrix(fm);
    info!("matrix: {}", matrix);

    (matrix, hash)
}
