use error::*;
use std::error::Error;

//--------------------------------------------------------------------------------------------------
/// error type
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct ET {
    message: String,
    detail: String
}

/// a very basic generic output wrapper
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct GOW<T> {
    ok: bool,
    data: Option<T>,
    error: Option<ET>
}

impl<T> GOW<T> /*where T: Ser*/ {
    pub fn ok(t: T) -> GOW<T> {
        GOW {
            ok: true,
            data: Some(t),
            error: None
        }
    }

    pub fn error(e: &ZError) -> GOW<T> {
        GOW {
            ok: false,
            data: None,
            error: Some(ET { message: e.description().to_owned(), detail: format!("{}", e) })
        }
    }
}

impl<T> From<ZError> for GOW<T> {
    fn from(e: ZError) -> Self { GOW::error(&e) }
}

impl<'t, T> From<&'t ZError> for GOW<T> {
    fn from(e: &'t ZError) -> Self { GOW::error(e) }
}

//--------------------------------------------------------------------------------------------------
// decoupled structures

/// error wrapper
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct EW {
    ok: bool,
    error: ET
}

impl EW {
    pub fn new(e: &ZError) -> Self {
        EW {
            ok: false,
            error: ET { message: e.description().to_owned(), detail: format!("{}", e) }
        }
    }

    pub fn new_s(m: String, d: String) -> Self {
        EW {
            ok: false,
            error: ET { message: m, detail: d }
        }
    }
}

impl From<ZError> for EW {
    fn from(e: ZError) -> Self { EW::new(&e) }
}

impl<'t> From<&'t ZError> for EW {
    fn from(e: &'t ZError) -> Self { EW::new(e) }
}

/// generic output
#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub struct OW<T> {
    ok: bool,
    data: T
}

impl<T> OW<T> {
    pub fn new(t: T) -> Self {
        OW {
            ok: true,
            data: t
        }
    }
}