use cop::fof::{Form, SForm, SUnfold, SkolemState};
use cop::lean::{Clause, Matrix};
use cop::role::{Role, RoleMap};
use cop::term::Args;
use cop::{Lit, Offset, Signed};
use log::info;
use tptp::{top, TPTPIterator};

fn main() {
    env_logger::init();
    let mut forms = RoleMap::<Vec<SForm>>::default();
    parse_file("problems/skolem.p", &mut forms);
    let fm = forms.join().unwrap();
    info!("joined: {}", fm);
    let fm = if fm.contains_eqtm() {
        let preds = fm.predicates().collect();
        let consts = fm.constants().collect();
        let axioms = Form::eq_axioms(preds, consts);
        fm.add_premise(axioms.map_vars(&mut |v| v.to_string()))
    } else {
        fm
    };
    info!("equalised: {}", fm);
    let unfolds: [SUnfold; 4] = [
        Box::new(|fm| fm.unfold_neg()),
        Box::new(|fm| fm.unfold_impl()),
        Box::new(|fm| fm.unfold_eqfm_nonclausal()),
        Box::new(|fm| fm.unfold_eq_tm(&"=".to_string())),
    ];
    let fm = fm.fix(&|fm| fm.apply_unfolds(&unfolds));
    info!("unfolded: {}", fm);
    let fm = (-fm).nnf();
    info!("nnf: {}", fm);
    let fm: Form<_, usize> = fm.univar(Default::default(), &mut 0);
    info!("enumerated: {}", fm);
    let fm = fm.skolem_outer(&mut SkolemState::new(("skolem".to_string(), 0)));
    info!("skolemised: {}", fm);
    let (fm, _paths) = fm.order();
    info!("ordered: {}", fm);
    let fm = fm.cnf();
    info!("cnf: {}", fm);
    let mut matrix: Matrix<Lit<Signed<String>, _>> = Matrix::from(fm);
    info!("matrix: {}", matrix);

    let hash = Form::Atom("#".to_string(), Args::new());
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
    for lim in 1.. {
        let opt = Opt { cut: true, lim };
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
                parse_file(&filename, forms)
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

fn parse_file(filename: &str, forms: &mut RoleMap<Vec<SForm>>) {
    info!("loading {}", filename);
    let bytes = std::fs::read(filename).unwrap();
    parse_bytes(&bytes, forms)
}
