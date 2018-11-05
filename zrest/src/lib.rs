#[macro_use] extern crate log;
extern crate futures;
extern crate hyper;
extern crate http;
extern crate tokio_tcp;
extern crate hyper_tls;
extern crate serde;
extern crate native_tls;
extern crate tokio_tls;
extern crate mime;
extern crate base64;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate serde_json;
#[cfg(test)] extern crate tokio;
#[cfg(test)] extern crate env_logger;

mod uri_scanner;
#[macro_use] pub mod error;
mod common;
mod data_io;
pub mod auth;
pub mod client;
mod tls_server;
#[macro_use] mod router;
#[macro_use] pub mod server;
//pub mod wrappers;

