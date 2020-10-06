mod fof;
mod role;
mod term;

use log::info;
use fof::{SForm, SUnfold};
use role::{Role, RoleMap};
use tptp::parsers::TPTPIterator;
use tptp::syntax;

fn main() {
    env_logger::init();
    let mut forms = RoleMap::<Vec<SForm>>::default();
    parse_file("problems/skolem.p", &mut forms);
    let form = forms.join().unwrap();
    let unfolds: [SUnfold; 3] = [
        Box::new(|fm| fm.unfold_neg()),
        Box::new(|fm| fm.unfold_impl()),
        Box::new(|fm| fm.unfold_eqfm_nonclausal()),
    ];
    let prematrix = form.prematrix(&|fm| fm.apply_unfolds(&unfolds), &mut 0);
    info!("prematrix: {}", prematrix);
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
