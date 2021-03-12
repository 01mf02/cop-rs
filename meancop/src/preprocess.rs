use crate::{Cli, Error};
use colosseum::unsync::Arena;
use cop::fof::{Form, Op, SForm, SkolemState};
use cop::lean::Matrix;
use cop::term::Args;
use cop::{change, ptr, szs};
use cop::{Lit, Signed, Symbol};
use log::info;

fn add_eq_axioms(fm: SForm) -> Result<SForm, Error> {
    let (preds, consts) = fm.predconst_unique();
    info!("predicates: {:?}", preds);
    info!("constants: {:?}", consts);

    // check that all symbols occur with the same arities
    let nfpreds: Vec<_> = cop::nonfunctional(preds.clone()).collect();
    let nfconsts: Vec<_> = cop::nonfunctional(consts.clone()).collect();
    if !nfpreds.is_empty() || !nfconsts.is_empty() {
        let s = format!("Arity mismatch for {:?} / {:?}", nfpreds, nfconsts);
        return Err(Error::new(szs::SyntaxError, s.into()));
    }

    Ok(if fm.subforms().any(|fm| matches!(fm, Form::EqTm(_, _))) {
        let axioms = Form::eq_axioms(preds, consts);
        fm.add_premise(axioms.map_vars(&mut |v| v.to_string()))
    } else {
        fm
    })
}

type Cnf = Form<String, String, usize>;

fn cnf(fm: SForm, cli: &Cli) -> Cnf {
    let unfolds: [Box<change::DynFn<SForm>>; 4] = [
        Box::new(|fm| fm.unfold_neg()),
        Box::new(|fm| fm.unfold_impl()),
        Box::new(|fm| fm.unfold_eqfm_disj_conj()),
        Box::new(|fm| fm.unfold_eq_tm(&"=".to_string())),
    ];
    let fm = fm.fix(&|fm| change::fold(fm, &unfolds));
    info!("unfolded: {}", fm);
    let fm = (-fm).nnf();
    info!("nnf: {}", fm);
    let fm: Form<_, _, usize> = fm.fresh_vars(&mut Default::default(), &mut 0);
    info!("fresh vars: {}", fm);
    let fm = fm.skolem_outer(&mut SkolemState::new(("skolem".to_string(), 0)));
    info!("skolemised: {}", fm);
    let fm = if cli.nopaths { fm } else { fm.order().0 };
    info!("ordered: {}", fm);
    fm.cnf()
}

type Sign<'a> = Form<cop::Signed<cop::Symbol<'a>>, cop::Symbol<'a>, usize>;

fn sign(fm: Cnf, hash: Cnf, arena: &Arena<String>) -> (Sign, Sign) {
    let mut set = Default::default();
    let mut symb = |s| Symbol::new(ptr::normalise(s, arena, &mut set));
    let mut sign = |p| Signed::from(symb(p));

    let fm = fm.map_predicates(&mut sign);
    let hash = hash.map_predicates(&mut sign);

    let fm = fm.map_constants(&mut symb);
    let hash = hash.map_constants(&mut symb);

    (fm, hash)
}

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
    cli: &Cli,
    arena: &'a Arena<String>,
) -> Result<(Matrix<SLit<'a>>, Sign<'a>), Error> {
    let fm = add_eq_axioms(fm)?;
    info!("equalised: {}", fm);

    // "#" marks clauses stemming from the conjecture
    // we can interpret it as "$true"
    let hash = Form::Atom("#".to_string(), Args::new());
    let (hashed, fm) = match (cli.conj, fm) {
        (true, Form::Bin(a, Op::Impl, c)) => {
            (true, Form::imp(*a & hash.clone(), hash.clone() & *c))
        }
        (_, fm) => (false, fm),
    };
    info!("hashed: {}", fm);

    let fm = cnf(fm, cli);
    let hash = hash.map_vars(&mut |_| 0);
    info!("cnf: {}", fm);

    let (fm, hash) = sign(fm, hash, arena);

    let mut matrix = matrix(fm);
    info!("matrix: {}", matrix);

    if !hashed {
        hash_matrix(&mut matrix, &hash)
    }
    info!("hashed: {}", matrix);
    Ok((matrix, hash))
}
