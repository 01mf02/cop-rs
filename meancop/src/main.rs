use clap::Clap;
use colosseum::unsync::Arena;
use cop::fof::{Form, Op, SForm, SkolemState};
use cop::lean::{Clause, Matrix};
use cop::role::RoleMap;
use cop::term::Args;
use cop::{change, ptr, szs};
use cop::{Lit, Offset, Signed, Symbol};
use log::info;
use meancop::{parse, Cli, Error};
use std::fs::File;
use std::io::Write;

fn main() {
    use env_logger::Env;
    // log warnings and errors by default, do not print timestamps
    env_logger::from_env(Env::default().filter_or("LOG", "warn"))
        .format_timestamp(None)
        .init();

    let cli = Cli::parse();
    let arena: Arena<String> = Arena::new();

    let result = run(&cli, &arena);
    if let Err(e) = result {
        print!("{}", szs::Status(e.get_kind()));
        if let Some(e) = e.get_error() {
            cli.output(e).unwrap()
        };
        std::process::exit(1);
    }
}

fn run(cli: &Cli, arena: &Arena<String>) -> Result<(), Error> {
    let mut forms = RoleMap::<Vec<SForm>>::default();
    parse::parse_file(&cli.file, &mut forms)?;
    let fm = match forms.join() {
        Some(fm) => fm,
        None => {
            print!("{}", szs::Status(szs::Satisfiable));
            return Ok(());
        }
    };
    info!("joined: {}", fm);

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

    let fm = if fm.subforms().any(|fm| matches!(fm, Form::EqTm(_, _))) {
        let axioms = Form::eq_axioms(preds, consts);
        fm.add_premise(axioms.map_vars(&mut |v| v.to_string()))
    } else {
        fm
    };
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
    let fm = fm.cnf();
    info!("cnf: {}", fm);

    let mut set = Default::default();
    let mut symb = |s| Symbol::new(ptr::normalise(s, arena, &mut set));
    let mut sign = |p| Signed::from(symb(p));

    let hash = hash.map_predicates(&mut sign);
    let fm = fm.map_predicates(&mut sign);

    let hash = hash.map_constants(&mut symb);
    let fm = fm.map_constants(&mut symb);

    let hash = hash.map_vars(&mut |_| 0);

    let matrix = Matrix::from(fm);
    info!("matrix: {}", matrix);
    let mut matrix: Matrix<_> = matrix
        .into_iter()
        .filter(|cl| !cl.is_trivial())
        .map(|cl| cl.fresh_vars(&mut Default::default(), &mut 0))
        .collect();
    info!("filter & fresh: {}", matrix);

    if !hashed {
        // insert "#" at the beginning of every positive clause
        for cl in matrix.iter_mut() {
            if cl.iter().all(|lit| lit.head().is_sign_negative()) {
                cl.insert(0, Lit::from(-hash.clone()))
            }
        }
    }
    info!("hashed: {}", matrix);

    let db = matrix.contrapositives().collect();
    info!("db: {}", db);
    let start = Clause::from(hash.clone());
    let start = Offset::new(0, &start);
    use cop::lean::search::{Context, Opt, Search, Task};
    let depths: Box<dyn Iterator<Item = _>> = match cli.lim {
        Some(lim) => Box::new(1..lim),
        None => Box::new(1..),
    };
    let mut infs_file = match &cli.infs {
        Some(file) => Some(File::create(file)?),
        None => None,
    };
    let cuts = cli.get_cuts();
    for lim in depths {
        info!("search with depth {}", lim);
        let opt = Opt { cuts, lim };
        let mut search = Search::new(Task::new(start), &db, opt);
        let proof = search.prove();
        let infs = search.inferences();
        info!("depth {} completed after {} inferences", lim, infs);
        if let Some(ref mut f) = infs_file {
            writeln!(f, r#"{{ "pathlim" : {} , "inferences" : {} }}"#, lim, infs)?;
        };
        if let Some(proof) = proof {
            let hash = Lit::from(hash.clone());
            let hash = Offset::new(0, &hash);
            assert!(proof.check(&search.sub, hash, Context::default()));
            print!("{}", szs::Status(szs::Theorem));
            let proof = proof.display(hash);
            cli.output(proof)?;
            return Ok(());
        }
    }

    Err(Error::from(szs::Incomplete))
}
