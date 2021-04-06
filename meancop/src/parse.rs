use crate::Error;
use cop::fof::SForm;
use cop::role::{Role, RoleMap};
use cop::szs;
use log::info;
use std::path::{Path, PathBuf};
use tptp::{top, TPTPIterator};

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

fn read_file(filename: &Path) -> std::io::Result<Vec<u8>> {
    std::fs::read(filename).or_else(|e| {
        let tptp = std::env::var("TPTP").or(Err(e))?;
        let mut path = PathBuf::from(tptp);
        path.push(filename);
        std::fs::read(path)
    })
}

pub fn parse_file(filename: &Path, forms: &mut RoleMap<Vec<SForm>>) -> Result<(), Error> {
    info!("loading {:?}", filename);
    let bytes = read_file(filename)?;
    parse_bytes(&bytes, forms)
}
