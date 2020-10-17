use cop::fof::{Form, SForm, SUnfold};
use cop::lean::{Db, Matrix};
use cop::role::{Role, RoleMap};
use log::info;
use tptp::parsers::TPTPIterator;
use tptp::syntax;

fn main() {
    env_logger::init();
    let mut forms = RoleMap::<Vec<SForm>>::default();
    parse_file("problems/skolem.p", &mut forms);
    let fm = forms.join().unwrap();
    info!("joined: {}", fm);
    let unfolds: [SUnfold; 3] = [
        Box::new(|fm| fm.unfold_neg()),
        Box::new(|fm| fm.unfold_impl()),
        Box::new(|fm| fm.unfold_eqfm_nonclausal()),
    ];
    let fm = fm.fix(&|fm| fm.apply_unfolds(&unfolds));
    info!("unfolded: {}", fm);
    let fm = fm.neg().nnf();
    info!("nnf: {}", fm);
    let fm: Form<_, usize> = fm.univar(Default::default(), &mut 0);
    info!("enumerated: {}", fm);
    let mut fresh = ("skolem".to_string(), 0);
    let fm = fm.skolem_outer(&mut Vec::new(), &mut Default::default(), &mut fresh);
    info!("skolemised: {}", fm);
    let (fm, _paths) = fm.order();
    info!("ordered: {}", fm);
    let fm = fm.cnf();
    info!("cnf: {}", fm);
    let matrix = Matrix::from(fm);
    info!("matrix: {}", matrix);
    let db: Db<_, _> = matrix.into_db().collect();
    info!("db: {}", db);
}

fn get_role_formula(annotated: syntax::AnnotatedFormula) -> (Role, SForm) {
    use syntax::AnnotatedFormula::*;
    match annotated {
        Fof(fof) => (Role::from(fof.role), SForm::from(*fof.formula)),
        Cnf(_cnf) => todo!(), //(Role::from(cnf.role), todo!()),
    }
}

fn parse_bytes(bytes: &[u8], forms: &mut RoleMap<Vec<SForm>>) {
    let mut parser = TPTPIterator::<()>::new(&bytes);
    for input in &mut parser {
        let input = input.expect("syntax error");
        match input {
            syntax::TPTPInput::Include(include) => {
                let filename = (include.file_name.0).0.to_string();
                parse_file(&filename, forms)
            }
            syntax::TPTPInput::Annotated(ann) => {
                let (role, formula) = get_role_formula(ann);
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
