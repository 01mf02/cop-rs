use cop::role::RoleMap;
use cop::szs::NoSuccessKind;
use cop::tptp::{parse, SForm};
use log::info;
use std::path::{Path, PathBuf};

fn read_file(filename: &Path) -> std::io::Result<Vec<u8>> {
    std::fs::read(filename).or_else(|e| {
        let tptp = std::env::var("TPTP").or(Err(e))?;
        let mut path = PathBuf::from(tptp);
        path.push(filename);
        std::fs::read(path)
    })
}

pub fn parse_file(filename: &Path, forms: &mut RoleMap<Vec<SForm>>) -> Result<(), NoSuccessKind> {
    info!("loading {:?}", filename);
    let bytes = read_file(filename).map_err(|_| NoSuccessKind::InputError)?;
    parse(&bytes, forms, |filename, forms| {
        parse_file(&PathBuf::from(filename), forms)
    })
}
