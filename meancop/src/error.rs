use cop::szs::{self, NoSuccessKind};

pub struct Error(NoSuccessKind, Option<Box<dyn std::error::Error>>);

impl Error {
    pub fn new(k: NoSuccessKind, e: Box<dyn std::error::Error>) -> Self {
        Self(k, Some(e))
    }

    pub fn get_kind(&self) -> &NoSuccessKind {
        &self.0
    }

    pub fn get_error(&self) -> &Option<Box<dyn std::error::Error>> {
        &self.1
    }
}

impl From<NoSuccessKind> for Error {
    fn from(k: NoSuccessKind) -> Self {
        Self(k, None)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Self::new(szs::OsError, e.into())
    }
}
