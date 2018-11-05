use futures::{
    future::{self, Future}
};
use std::{borrow::Cow, ops::Add};
use hyper::{
    Body, Request, Response, StatusCode,
};
use error::*;

pub type ZResult<T> = Result<T, ZError>;

// Just a simple type alias
pub type BFR = Box<Future<Item=Response<Body>, Error=ZError> + Send>;

pub fn flat_response(status: StatusCode, msg: Cow<'static, str>) -> Response<Body> {
    let mut response = Response::new(Body::from(msg));
    *response.status_mut() = status;
    response
}

pub fn notimplemented_response() -> Response<Body> {
    flat_response(StatusCode::NOT_IMPLEMENTED, Cow::from("default pipeline invoked"))
}


pub struct QAttr {
    pub user: Option<String>,
    pub proxy_user: Option<String>
}

impl QAttr {
    pub fn new() -> QAttr {
        QAttr { user: None, proxy_user: None }
    }
    pub fn user(self, user: String) -> QAttr {
        QAttr { user: Some(user), ..self }
    }
    pub fn proxy_user(self, user: String) -> QAttr {
        QAttr { proxy_user: Some(user), ..self }
    }
    pub fn xuser(self, user: String, is_proxy: bool) -> QAttr {
        if is_proxy {
            self.proxy_user(user)
        } else {
            self.user(user)
        }
    }
}

pub struct Pipeline {
    h: Box<Fn(Request<Body>, QAttr) -> BFR + Send>
}

impl Pipeline {
    pub fn new(h: impl Fn(Request<Body>, QAttr) -> BFR + Send + 'static) -> Pipeline {
        Pipeline { h: Box::new(h) }
    }

    pub fn default() -> Pipeline {
        Self::new(|_, _| Box::new(future::ok(notimplemented_response())))
    }

    pub fn apply(&self, r: Request<Body>, a: QAttr) -> BFR {
        (self.h)(r, a)
    }
}

pub trait PipelineTransform: Send + 'static {
    fn apply(&self, input: Pipeline) -> Pipeline;
}

impl<T1, T2> PipelineTransform for (T1, T2)
    where T1: PipelineTransform, T2: PipelineTransform {
    fn apply(&self, input: Pipeline) -> Pipeline {
        self.1.apply(self.0.apply(input))
    }
}

pub struct FormatErrors;

impl PipelineTransform for FormatErrors {
    fn apply(&self, h: Pipeline) -> Pipeline {
        Pipeline::new(move |r, q| Box::new(h.apply(r, q).then(|rsp| match rsp {
            Ok(ok) => Ok(ok),
            Err(ZError::AppHttp(sc, m)) => Ok(flat_response(sc, m)),
            Err(other) => Ok(flat_response(StatusCode::INTERNAL_SERVER_ERROR, Cow::from(format!("Internal error: {}", other))))
        })))
    }
}


pub use serde::ser::Serialize as Ser;
pub use serde::de::DeserializeOwned as Des;

