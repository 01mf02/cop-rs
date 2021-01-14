use clap::Clap;
use colosseum::unsync::Arena;
use cop::fof::{Form, Op, SForm, SkolemState};
use cop::lean::{Clause, Matrix};
use cop::role::{Role, RoleMap};
use cop::term::Args;
use cop::{change, ptr, szs};
use cop::{Lit, Offset, Signed, Symbol};
use log::info;
use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{self, Write};
use std::path::PathBuf;
use tptp::{top, TPTPIterator};

/// Automated theorem prover for first-order logic with equality
///
/// This prover aims to explore
/// efficient implementation techniques for both
/// clausal and nonclausal connection calculi.
///
/// Set the environment variable "LOG" to "info", "debug", or "trace"
/// to obtain an increasingly detailed log.
#[derive(Clap)]
struct Cli {
    /// Disregard alternatives when a branch is closed
    ///
    /// There are two kinds of cuts: "shallow" and "deep".
    /// Deep cut is what is implemented in leanCoP as "cut".
    /// The two cut types differ in their behaviour when
    /// a branch (including all its ancestors) is closed:
    /// Deep cut excludes any possibility of closing that branch differently, whereas
    /// shallow cut only permits for different extension steps at the branch root.
    ///
    /// This option makes the search incomplete!
    #[clap(long)]
    cut: Option<cop::lean::search::Cut>,

    /// Enable conjecture-directed proof search
    #[clap(long)]
    conj: bool,

    /// Disable matrix sorting by number of paths
    #[clap(long)]
    nopaths: bool,

    /// Maximal depth for iterative deepening
    #[clap(long)]
    lim: Option<usize>,

    /// Write SZS output (such as proofs and error details) to given file
    #[clap(short)]
    output: Option<PathBuf>,

    /// Write inference statistics in JSON format to given file
    #[clap(long)]
    infs: Option<PathBuf>,

    /// Path of the TPTP problem file
    file: PathBuf,
}

struct Error(szs::NoSuccessKind, Option<Box<dyn std::error::Error>>);

impl Error {
    fn new(k: szs::NoSuccessKind, e: Box<dyn std::error::Error>) -> Self {
        Self(k, Some(e))
    }
}

impl From<szs::NoSuccessKind> for Error {
    fn from(k: szs::NoSuccessKind) -> Self {
        Self(k, None)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::new(szs::OSError, e.into())
    }
}

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
        print!("{}", szs::Status(e.0));
        if let Some(e) = e.1 {
            match cli.output {
                Some(o) => fs::write(o, e.to_string()).unwrap(),
                None => print!("{}", szs::Output(e)),
            }
        };
        std::process::exit(1);
    }
}

fn run(cli: &Cli, arena: &Arena<String>) -> Result<(), Error> {
    let mut forms = RoleMap::<Vec<SForm>>::default();
    parse_file(&cli.file, &mut forms)?;
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

    let mut set: HashSet<&str> = HashSet::new();
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
    for lim in depths {
        info!("search with depth {}", lim);
        let opt = Opt { cut: cli.cut, lim };
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
            match &cli.output {
                Some(o) => fs::write(o, proof.to_string())?,
                None => print!("{}", szs::Output(proof)),
            };
            return Ok(());
        }
    }

    Err(Error::from(szs::Incomplete))
}

fn get_role_formula(annotated: top::AnnotatedFormula) -> (Role, SForm) {
    use top::AnnotatedFormula::*;
    match annotated {
        Fof(fof) => (Role::from(fof.0.role), SForm::from(*fof.0.formula)),
        Cnf(_cnf) => todo!(), //(Role::from(cnf.role), todo!()),
    }
}

fn parse_bytes(bytes: &[u8], forms: &mut RoleMap<Vec<SForm>>) -> Result<(), Error> {
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        let input = input.map_err(|_| Error::from(szs::SyntaxError))?;
        match input {
            top::TPTPInput::Include(include) => {
                let filename = (include.file_name.0).0.to_string();
                info!("include {}", filename);
                parse_file(&PathBuf::from(filename), forms)?
            }
            top::TPTPInput::Annotated(ann) => {
                let (role, formula) = get_role_formula(*ann);
                info!("loading formula: {}", formula);
                forms.get_mut(role).push(formula);
            }
        };
    }
    if parser.remaining.is_empty() {
        Ok(())
    } else {
        Err(Error::from(szs::SyntaxError))
    }
}

fn read_file(filename: &PathBuf) -> io::Result<Vec<u8>> {
    std::fs::read(filename.clone()).or_else(|e| {
        let tptp = std::env::var("TPTP").or(Err(e))?;
        let mut path = PathBuf::from(tptp);
        path.push(filename);
        std::fs::read(path)
    })
}

fn parse_file(filename: &PathBuf, forms: &mut RoleMap<Vec<SForm>>) -> Result<(), Error> {
    info!("loading {:?}", filename);
    let bytes = read_file(filename)?;
    parse_bytes(&bytes, forms)
}
