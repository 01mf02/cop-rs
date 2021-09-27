use clap::Clap;
use colosseum::unsync::Arena;
use cop::change;
use cop::fof::{Form, SkolemState};
use cop::lean::Matrix;
use cop::term::Args;
use cop::tptp::SForm;
use cop::{Lit, Signed};
use log::info;

#[derive(Clap)]
pub struct Options {
    /// Enable conjecture-directed proof search
    #[clap(long)]
    conj: bool,

    /// Disable matrix sorting by number of paths
    #[clap(long)]
    nopaths: bool,
}

fn unfolds() -> [Box<change::DynFn<SForm>>; 4] {
    [
        Box::new(|fm| fm.unfold_neg()),
        Box::new(|fm| fm.unfold_impl()),
        Box::new(|fm| fm.unfold_eqfm_disj_conj()),
        Box::new(|fm| fm.unfold_eq_tm("=".to_string())),
    ]
}

type Sign<'a> = Form<cop::Signed<cop::Symbol<'a>>, cop::Symbol<'a>, usize>;

type SLit<'a> = cop::Lit<cop::Signed<cop::Symbol<'a>>, cop::Symbol<'a>, usize>;

fn matrix(fm: Sign) -> Matrix<SLit> {
    let matrix = Matrix::from(fm);
    matrix
        .into_iter()
        .filter(|cl| !cl.is_trivial())
        .map(|cl| cl.fresh_vars(&mut Default::default(), &mut 0))
        .collect()
}

fn hash_matrix<'a>(matrix: &mut Matrix<SLit<'a>>, hash: &Sign<'a>) {
    // insert "#" at the beginning of every positive clause
    for cl in matrix.iter_mut() {
        if cl.iter().all(|lit| lit.head().is_sign_negative()) {
            cl.insert(0, Lit::from(-hash.clone()))
        }
    }
}

pub fn preprocess<'a>(
    fm: SForm,
    opts: &Options,
    arena: &'a Arena<String>,
) -> (Matrix<SLit<'a>>, Sign<'a>) {
    // "#" marks clauses stemming from the conjecture
    // we can interpret it as "$true"
    let hash = Form::Atom("#".to_string(), Args::new());
    let (hashed, fm) = change::and_then(opts.conj, fm, |fm| fm.mark_impl(&hash));
    info!("hashed: {}", fm);

    let fm = fm.fix(&|fm| change::fold(fm, &unfolds()));
    info!("unfolded: {}", fm);
    let fm = (-fm).nnf();
    info!("nnf: {}", fm);
    let fm: Form<_, _, usize> = fm.fresh_vars(&mut Default::default(), &mut 0);
    let hash = hash.map_vars(&mut |_| 0);
    info!("fresh vars: {}", fm);
    let fm = fm.skolem_outer(&mut SkolemState::new(("skolem".to_string(), 0)));
    info!("skolemised: {}", fm);
    let fm = if opts.nopaths { fm } else { fm.order().0 };
    info!("ordered: {}", fm);
    let fm = fm.cnf();
    info!("cnf: {}", fm);

    let mut set = Default::default();
    let sign = &mut Signed::from;
    let fm = fm.symbolise(&mut set, arena).map_predicates(sign);
    let hash = hash.symbolise(&mut set, arena).map_predicates(sign);

    let mut matrix = matrix(fm);
    info!("matrix: {}", matrix);

    if !hashed {
        hash_matrix(&mut matrix, &hash)
    }
    info!("hashed: {}", matrix);
    (matrix, hash)
}
