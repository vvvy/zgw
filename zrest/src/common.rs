use futures::{
    future::{Future}
};
use std::borrow::Cow;
use hyper::{
    Body, Response, StatusCode,
};
use error::*;

pub type ZResult<T> = Result<T, ZError>;

// Just a simple type alias
pub type BoxFut = Box<Future<Item=Response<Body>, Error=ZError> + Send>;

pub fn flat_response(status: StatusCode, msg: Cow<'static, str>) -> Response<Body> {
    let mut response = Response::new(Body::from(msg));
    *response.status_mut() = status;
    response
}

pub use serde::ser::Serialize as Ser;
pub use serde::de::DeserializeOwned as Des;

