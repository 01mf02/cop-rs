use clap::Clap;
use colosseum::unsync::Arena;
use cop::change;
use cop::fof::{Cnf, Fof, SkolemState};
use cop::lean::Matrix;
use cop::term::Args;
use cop::tptp::SFof;
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

type SLit<'a> = cop::Lit<cop::Signed<cop::Symbol<'a>>, cop::Symbol<'a>, usize>;

fn matrix(fm: Cnf<SLit>) -> Matrix<SLit> {
    let matrix = Matrix::from(fm);
    matrix
        .into_iter()
        .filter(|cl| !cl.is_trivial())
        .map(|cl| cl.fresh_vars(&mut Default::default(), &mut 0))
        .collect()
}

fn hash_matrix<'a>(matrix: &mut Matrix<SLit<'a>>, hash: &SLit<'a>) {
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
    // "#" marks clauses stemming from the conjecture
    // we can interpret it as "$true"
    let hash = Fof::atom("#".to_string(), Args::new());
    let (hashed, fm) = change::and_then(opts.conj, fm, |fm| fm.mark_impl(|| hash.clone()));
    info!("hashed: {}", fm);

    let fm = fm.map_atoms(&mut |a| a.to_lit(|| "=".to_string()).map_head(Signed::from));
    let fm = fm.qnnf();
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

    let mut matrix = matrix(fm);
    info!("matrix: {}", matrix);

    if !hashed {
        hash_matrix(&mut matrix, &hash)
    }
    info!("hashed: {}", matrix);
    (matrix, hash)
}
