use std::{
    collections::HashMap,
    borrow::Cow,
    sync::Arc
};
use futures::{
    future
};
use base64;
use hyper::{
    Body, Request, Response, StatusCode,
    header::{HeaderName, HeaderValue, PROXY_AUTHORIZATION, PROXY_AUTHENTICATE, AUTHORIZATION, WWW_AUTHENTICATE}
};
use http::request::Builder;
use common::*;
use error::*;


struct AuthType {
    is_proxy: bool,
    az: HeaderName,
    an: HeaderName,
    sc: StatusCode,
    m0: &'static str,
    m1: &'static str
}

impl AuthType {
    fn server() -> AuthType { AuthType {
        is_proxy: false,
        az: AUTHORIZATION,
        an: WWW_AUTHENTICATE,
        sc: StatusCode::UNAUTHORIZED,
        m0: "Authentication required",
        m1: "invalid username or password"
    } }
    fn proxy() -> AuthType { AuthType {
        is_proxy: true,
        az: PROXY_AUTHORIZATION,
        an: PROXY_AUTHENTICATE,
        sc: StatusCode::PROXY_AUTHENTICATION_REQUIRED,
        m0: "proxy authentication required",
        m1: "(proxy) invalid username or password"
    } }
    fn failresp(&self, anv: &HeaderValue) -> Response<Body> {
        let mut r = flat_response(self.sc, Cow::from(self.m0));
        r.headers_mut().insert(self.an.clone(), anv.clone());
        r
    }
}



#[derive(Clone)]
pub struct BasicAuth {
    auths: HashMap<Vec<u8>, String>,
    anv: HeaderValue
}

impl BasicAuth {
    pub fn new(realm: &str) -> Result<BasicAuth, ZError> {
        let anv = HeaderValue::from_bytes(format!("Basic realm=\"{}\"", realm).as_bytes())
            .map_err(|e| rest_error!(other "Invalid realm name: \"{}\" / {}", realm, e))?;
        Ok(BasicAuth { auths: HashMap::new(), anv })
    }

    pub fn add_user(&mut self, username: &str, password: &str) {
        self.auths.insert(
            base64::encode(&format!("{}:{}", username, password)).as_bytes().to_vec(),
            username.to_owned()
        );
    }

    pub fn run_client(mut self) -> Box<Fn(&mut Builder) + Send> {
        let v: Box<Fn(&mut Builder) + Send> = match self.auths.drain().next() {
            Some((v, _u)) => {
                let hv = format!("Basic {}", String::from_utf8_lossy(&v));
                Box::new(move |builder: &mut Builder| {
                    builder.header(AUTHORIZATION, hv.clone());
                })
            }
            None => Box::new(|_| ())
        };
        v
    }

    /*
    pub fn run_server(self, handler: Pipeline) -> Pipeline {
        self.run_serverside(AuthType::server(), handler)
    }
    pub fn run_proxy(self, handler: Pipeline) -> Pipeline {
        self.run_serverside(AuthType::proxy(), handler)
    }*/

    fn run_serverside(s: Arc<BasicAuth>, at: Arc<AuthType>, handler: Pipeline) -> Pipeline {
        Pipeline::new(move |req, qattr| match
            if let Some(azv) = req.headers().get(at.az.clone()) {
                let bs = azv.as_bytes();
                let user = if &bs[0..6] == b"Basic " {
                    s.auths.get(&bs[6..].to_vec())
                } else {
                    None
                };
                if user.is_some() {
                    trace!("Authentication succeeded");
                } else {
                    error!("Authentication failed: '{:?}'", azv);
                }
                user
            } else {
                trace!("Authentication: no data or invalid mech");
                None
            }
            {
                Some(u) => handler.apply(req, qattr.xuser(u.clone(), at.is_proxy)),
                None => Box::new(future::ok(at.failresp(&s.anv)))
            }
        )
    }
}

#[derive(Clone)]
pub struct BasicAuthTransform {
    ba: Arc<BasicAuth>,
    at: Arc<AuthType>
}

impl BasicAuthTransform {
    fn new(ba: BasicAuth, at: AuthType) -> BasicAuthTransform {
        BasicAuthTransform { ba: Arc::new(ba), at: Arc::new(at) }
    }
}

impl PipelineTransform for BasicAuthTransform {
    fn apply(&self, upstream: Pipeline) -> Pipeline {
        BasicAuth::run_serverside(self.ba.clone(), self.at.clone(), upstream)
    }
}