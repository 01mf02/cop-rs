use cop::szs::NoSuccessKind;
use std::path::{Path, PathBuf};

type RoleMap = cop::role::RoleMap<Vec<cop::tptp::SFof>>;

fn read_file(filename: &Path) -> std::io::Result<Vec<u8>> {
    std::fs::read(filename).or_else(|e| {
        let tptp = std::env::var("TPTP").or(Err(e))?;
        let mut path = PathBuf::from(tptp);
        path.push(filename);
        std::fs::read(path)
    })
}

pub fn parse_mut(filename: &Path, forms: &mut RoleMap) -> Result<(), NoSuccessKind> {
    log::info!("loading {:?}", filename);
    let bytes = read_file(filename).map_err(|_| NoSuccessKind::InputError)?;
    cop::tptp::parse(&bytes, forms, |filename, forms| {
        parse_mut(&PathBuf::from(filename), forms)
    })
}

pub fn parse(path: &Path) -> Result<RoleMap, NoSuccessKind> {
    let mut forms = Default::default();
    parse_mut(path, &mut forms)?;
    Ok(forms)
}
