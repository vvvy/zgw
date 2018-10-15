use std::collections::HashSet;
use std::borrow::Cow;
use futures::{
    future
};
use base64;
use hyper::{
    Body, Request, StatusCode,
    header::{HeaderName, HeaderValue, PROXY_AUTHORIZATION, PROXY_AUTHENTICATE, AUTHORIZATION, WWW_AUTHENTICATE}
};
use http::request::Builder;
use common::*;
use error::*;


#[derive(Clone)]
pub struct BasicAuth {
    auths: HashSet<Vec<u8>>,
    pa_value: HeaderValue
}

impl BasicAuth {
    pub fn new(realm: &str) -> Result<BasicAuth, ZError> {
        let pa_value = HeaderValue::from_bytes(format!("Basic realm=\"{}\"", realm).as_bytes())
            .map_err(|e| rest_error!(other "Invalid realm name: \"{}\" / {}", realm, e))?;
        Ok(BasicAuth { auths: HashSet::new(), pa_value })
    }

    pub fn add_user(&mut self, username: &str, password: &str) -> bool {
        self.auths.insert(
            base64::encode(&format!("{}:{}", username, password)).as_bytes().to_vec()
        )
    }

    pub fn run_client(mut self) -> Box<Fn(&mut Builder) + Send> {
        let v: Box<Fn(&mut Builder) + Send> = match self.auths.drain().next() {
            Some(v) => {
                let hv = format!("Basic {}", String::from_utf8_lossy(&v));
                Box::new(move |builder: &mut Builder| {
                    builder.header(AUTHORIZATION, hv.clone());
                })
            }
            None => Box::new(|_| ())
        };
        v
    }
    pub fn run_server(self, handler: impl Fn(Request<Body>) -> BoxFut) -> impl Fn(Request<Body>) -> BoxFut {
        self.run_serverside(
            AUTHORIZATION,
            WWW_AUTHENTICATE,
            StatusCode::FORBIDDEN,
            handler)
    }
    pub fn run_proxy(self, handler: impl Fn(Request<Body>) -> BoxFut) -> impl Fn(Request<Body>) -> BoxFut {
        self.run_serverside(
            PROXY_AUTHORIZATION,
            PROXY_AUTHENTICATE,
            StatusCode::PROXY_AUTHENTICATION_REQUIRED,
            handler)
    }
    pub fn run_serverside(self, az: HeaderName, an: HeaderName, sc: StatusCode, handler: impl Fn(Request<Body>) -> BoxFut) -> impl Fn(Request<Body>) -> BoxFut {
        //let az = PROXY_AUTHORIZATION;
        //let an = PROXY_AUTHENTICATE;
        move |req| match
            if let Some(pa) = req.headers().get(az.clone()) {
                let bs = pa.as_bytes();
                if &bs[0..6] == b"Basic " && self.auths.contains(&bs[6..].to_vec()) {
                    trace!("Authentication succeeded");
                    None
                } else {
                    error!("Authentication failed: '{:?}'", pa);
                    Some(flat_response(StatusCode::FORBIDDEN, Cow::from("Access denied")))
                }
            } else {
                let mut r = flat_response(sc, Cow::from("proxy authentication required"));
                r.headers_mut().insert(an.clone(), self.pa_value.clone());
                Some(r)
            }
            {
                Some(r) => Box::new(future::ok(r)),
                None => handler(req)
            }
    }
}
