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
pub type Err = String;

/// Result type
pub type Result<T> = std::result::Result<T, Err>;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}

