use std::{self, error::Error, fmt::Display};
use hyper;
use hyper_tls;
use mime;
use serde_json;
use http;
use uri_scanner;

pub use std::borrow::Cow;

#[derive(Debug)]
pub enum ZError {
    Hyper(hyper::error::Error),
    HyperHeaderToStr(hyper::header::ToStrError),
    HyperTls(hyper_tls::Error),
    MimeFromStr(mime::FromStrError),
    SerdeJson(serde_json::Error),
    Http(http::Error),
    Io(std::io::Error),
    UriScanner(uri_scanner::SErr),
    InvalidUri(http::uri::InvalidUri),
    InvalidUriParts(http::uri::InvalidUriParts),
    Other(Cow<'static, str>)
}


impl Error for ZError {
    /*fn description(&self) -> &str {
        match self {
            ZError::Hyper(e) => e.description(),
            ZError::HyperHeaderToStr(e) => e.description(),
            ZError::HyperTls(e) => e.description(),
            ZError::MimeFromStr(e) => e.description(),
            ZError::SerdeJson(e) => e.description(),
            ZError::Http(e) => e.description(),
            ZError::Io(e) => e.description(),
            ZError::UriScanner(e) => e.description(),
            ZError::Other(s) => s
        }
    }*/

    fn cause(&self) -> Option<&dyn Error> {
        match self {
            ZError::Hyper(e) => e.cause(),
            ZError::HyperHeaderToStr(e) => e.cause(),
            ZError::HyperTls(e) => e.cause(),
            ZError::MimeFromStr(e) => e.cause(),
            ZError::SerdeJson(e) => e.cause(),
            ZError::Http(e) => e.cause(),
            ZError::Io(e) => e.cause(),
            ZError::UriScanner(e) => e.cause(),
            ZError::InvalidUri(e) => e.cause(),
            ZError::InvalidUriParts(e) => e.cause(),
            ZError::Other(_) => None
        }
    }

}

impl Display for ZError {
    fn fmt<'a>(&self, f: &mut std::fmt::Formatter<'a>) -> Result<(), std::fmt::Error> {
        match self {
            ZError::Hyper(e) => write!(f, "(Hyper): ").and_then(|_| e.fmt(f)),
            ZError::HyperHeaderToStr(e) => write!(f, "(HyperHeaderToStr): ").and_then(|_| e.fmt(f)),
            ZError::HyperTls(e) => write!(f, "(HyperTls): ").and_then(|_| e.fmt(f)),
            ZError::MimeFromStr(e) => write!(f, "(MimeFromStr): ").and_then(|_| e.fmt(f)),
            ZError::SerdeJson(e) => write!(f, "(SerdeJson): ").and_then(|_| e.fmt(f)),
            ZError::Http(e) => write!(f, "(Http): ").and_then(|_| e.fmt(f)),
            ZError::Io(e) => write!(f, "(Io): ").and_then(|_| e.fmt(f)),
            ZError::UriScanner(e) => write!(f, "(UriScanner): ").and_then(|_| e.fmt(f)),
            ZError::InvalidUri(e) => write!(f, "(InvalidUri): ").and_then(|_| e.fmt(f)),
            ZError::InvalidUriParts(e) => write!(f, "(InvalidUriParts): ").and_then(|_| e.fmt(f)),
            ZError::Other(s) => write!(f, "(Other): {}", s)
        }
    }
}

impl From<hyper::error::Error> for ZError {
    fn from(e: hyper::error::Error) -> Self {
        ZError::Hyper(e)
    }
}

impl From<http::Error> for ZError {
    fn from(e: http::Error) -> Self {
        ZError::Http(e)
    }
}

impl From<mime::FromStrError> for ZError {
    fn from(e: mime::FromStrError) -> Self {
        ZError::MimeFromStr(e)
    }
}

impl From<hyper::header::ToStrError> for ZError {
    fn from(e: hyper::header::ToStrError) -> Self {
        ZError::HyperHeaderToStr(e)
    }
}

impl From<hyper_tls::Error> for ZError {
    fn from(e: hyper_tls::Error) -> Self {
        ZError::HyperTls(e)
    }
}
impl From<serde_json::Error> for ZError {
    fn from(e: serde_json::Error) -> Self {
        ZError::SerdeJson(e)
    }
}
impl From<std::io::Error> for ZError {
    fn from(e: std::io::Error) -> Self {
        ZError::Io(e)
    }
}
impl From<uri_scanner::SErr> for ZError {
    fn from(e: uri_scanner::SErr) -> Self {
        ZError::UriScanner(e)
    }
}
impl From<http::uri::InvalidUri> for ZError {
    fn from(e: http::uri::InvalidUri) -> Self {
        ZError::InvalidUri(e)
    }
}
impl From<http::uri::InvalidUriParts> for ZError {
    fn from(e: http::uri::InvalidUriParts) -> Self {
        ZError::InvalidUriParts(e)
    }
}



#[macro_export]
macro_rules! rest_error {
    {other $e:expr} => { ZError::Other(Cow::from($e)) };
    {other $($es:expr),+} => { ZError::Other(Cow::from(format!($($es),+))) };
}
