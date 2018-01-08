#[macro_use]
extern crate log;

extern crate simplelog;

#[macro_use]
extern crate serde_derive;

extern crate serde;
extern crate serde_yaml;

#[macro_use]
pub mod macros;

pub mod nv;
pub mod nq;
pub mod w;
pub mod config;

/// Our universal error type
#[derive(Debug)]
pub enum Error {
    Gen(String),
    IO(String, std::io::Error),
    YAML(String, serde_yaml::Error),
    Other(String, Box<std::error::Error>)
}

impl Error {
    pub fn gen(s: &str) -> Error { Error::Gen(s.to_owned()) }
    pub fn io(s: &str, e: std::io::Error) -> Error { Error::IO(s.to_owned(), e) }
    pub fn yaml(s: &str, e: serde_yaml::Error) -> Error { Error::YAML(s.to_owned(), e) }
    pub fn other<E>(s: &str, e: E) -> Error where E: std::error::Error + 'static { Error::Other(s.to_owned(), Box::new(e)) }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Error::Gen(ref s) => write!(f, "Generic: {}", s),
            Error::IO(ref s, ref e) => write!(f, "I/O: {} ({})", s, e),
            Error::YAML(ref s, ref e) => write!(f, "YAML serde: {} ({})", s, e),
            Error::Other(ref s, ref e) => write!(f, "Other: {} ({})", s, e),
        }
    }
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::Gen(ref s) => s,
            Error::IO(ref s, _) => s,
            Error::YAML(ref s, _) => s,
            Error::Other(ref s, _) => s
        }
    }

    fn cause(&self) -> Option<&std::error::Error> {
        match *self {
            Error::Gen(_) => None,
            Error::IO(_, ref e) => Some(e),
            Error::YAML(_, ref e) => Some(e),
            Error::Other(_, ref e) => Some(e.as_ref())
        }
    }
}

/// Result type
pub type Result<T> = std::result::Result<T, Error>;

/*
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

*/