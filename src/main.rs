use clap::Clap;
use colosseum::unsync::Arena;
use cop::fof::{Form, SForm, SkolemState};
use cop::lean::{Clause, Matrix};
use cop::role::{Role, RoleMap};
use cop::term::Args;
use cop::{change, ptr};
use cop::{Lit, Offset, Signed, Symbol};
use log::info;
use std::collections::HashSet;
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
    /// Disregard alternatives when an extension step succeeds
    ///
    /// This option makes the search incomplete!
    #[clap(long)]
    cut: bool,

    /// Maximal depth for iterative deepening
    #[clap(long)]
    lim: Option<usize>,

    /// Path of the TPTP problem file
    file: PathBuf,
}

fn main() {
    use env_logger::Env;
    // log warnings and errors by default
    env_logger::from_env(Env::default().filter_or("LOG", "warn")).init();

    let cli = Cli::parse();

    let mut forms = RoleMap::<Vec<SForm>>::default();
    parse_file(cli.file, &mut forms);
    let fm = forms.join().unwrap();
    info!("joined: {}", fm);
    let fm = if fm.subforms().any(|fm| matches!(fm, Form::EqTm(_, _))) {
        let preds = fm.predicates().collect();
        let consts = fm.constants().collect();
        let axioms = Form::eq_axioms(preds, consts);
        fm.add_premise(axioms.map_vars(&mut |v| v.to_string()))
    } else {
        fm
    };
    info!("equalised: {}", fm);
    let unfolds: [Box<change::DynFn<SForm>>; 4] = [
        Box::new(|fm| fm.unfold_neg()),
        Box::new(|fm| fm.unfold_impl()),
        Box::new(|fm| fm.unfold_eqfm_nonclausal()),
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
    let (fm, _paths) = fm.order();
    info!("ordered: {}", fm);
    let fm = fm.cnf();
    info!("cnf: {}", fm);

    let arena: Arena<String> = Arena::new();
    let mut set: HashSet<&str> = HashSet::new();
    let mut symb = |s| Symbol::new(ptr::normalise(s, &arena, &mut set));
    let mut sign = |p| Signed::from(symb(p));
    let fm = fm.map_predicates(&mut sign);
    let fm = fm.map_constants(&mut symb);

    let matrix = Matrix::from(fm);
    info!("matrix: {}", matrix);
    let mut matrix: Matrix<_> = matrix
        .into_iter()
        .map(|cl| cl.fresh_vars(&mut Default::default(), &mut 0))
        .collect();
    info!("fresh vars: {}", matrix);

    let hash = Signed::from(symb("#".to_string()));
    let hash = Form::Atom(hash, Args::new());
    let hash_lit = Lit::from(-hash.clone());
    for cl in matrix.iter_mut() {
        if cl.iter().all(|lit| lit.head().is_sign_negative()) {
            // TODO: push at front for compatibility?
            cl.push(hash_lit.clone());
        }
    }

    let db = matrix.into_db().collect();
    info!("db: {}", db);
    let start = Clause::from(hash);
    let start = Offset::new(0, &start);
    use cop::lean::search::{Opt, State, Task};
    let depths: Box<dyn Iterator<Item = _>> = match cli.lim {
        Some(lim) => Box::new(1..lim),
        None => Box::new(1..),
    };
    for lim in depths {
        info!("search with depth {}", lim);
        let opt = Opt { cut: cli.cut, lim };
        let mut search = State::new(Task::new(start), &db, opt);
        if search.prove() {
            break;
        }
    }

    println!("SZS status theorem");
}

fn get_role_formula(annotated: top::AnnotatedFormula) -> (Role, SForm) {
    use top::AnnotatedFormula::*;
    match annotated {
        Fof(fof) => (Role::from(fof.0.role), SForm::from(*fof.0.formula)),
        Cnf(_cnf) => todo!(), //(Role::from(cnf.role), todo!()),
    }
}

fn parse_bytes(bytes: &[u8], forms: &mut RoleMap<Vec<SForm>>) {
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        let input = input.expect("syntax error");
        match input {
            top::TPTPInput::Include(include) => {
                let filename = (include.file_name.0).0.to_string();
                parse_file(PathBuf::from(filename), forms)
            }
            top::TPTPInput::Annotated(ann) => {
                let (role, formula) = get_role_formula(*ann);
                info!("loading formula: {}", formula);
                forms.get_mut(role).push(formula);
            }
        };
    }
    assert!(parser.remaining.is_empty());
}

fn parse_file(filename: PathBuf, forms: &mut RoleMap<Vec<SForm>>) {
    info!("loading {:?}", filename);
    let bytes = std::fs::read(filename).unwrap();
    parse_bytes(&bytes, forms)
}
