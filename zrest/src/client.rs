

use futures::{Future, Stream};
use tokio_core::reactor::Core;

use hyper::{Client, Request, Body, Uri, Method};
use hyper::header::{Authorization, Basic, ContentType, ContentLength};
use hyper::client::{HttpConnector, FutureResponse};
use hyper_tls::HttpsConnector;
use serde;
use serde_json;
use ::*;

pub trait CoreH {
    fn core(&self) -> &Core;
    fn core_mut(&mut self) -> &mut Core;

    fn recv_sync<R>(&mut self, fr: FutureResponse) -> ZRResult<R> where R: serde::de::DeserializeOwned {
        let work =
            fr
                .map_err(
                    |e| ZRError::from(e)
                ).and_then(|res|
                    match res.headers().get::<ContentType>() {
                        Some(rct) if rct.type_() == mime::APPLICATION && rct.subtype() == mime::JSON => Ok(()),
                        Some(rct) => Err(ZRError::EZR(format!("invalid content type `{}`", *rct))),
                        None => Err(ZRError::EZR(format!("no content type found (application/json required)")))
                    } .map(|_| res)
                ).and_then(|res|
                    res.body()
                        .concat2()
                        .map_err(|e| ZRError::from(e))
                        .and_then(move |body|
                            serde_json::from_slice(&body).map_err(|e| ZRError::from(e))
                        )
                );
        self.core_mut().run(work)
    }
}

pub trait ZClient : CoreH {
    /// Creates FutureResponse for a GET request
    fn get_fr(&self, uri: Uri) -> FutureResponse;

    /// Creates FutureResponse for a generic request
    fn req_fr(&self, req: Request<Body>) -> FutureResponse;

    /// Performs sync GET
    #[inline]
    fn get<R>(&mut self, uri: Uri) -> ZRResult<R> where R: serde::de::DeserializeOwned {
        let x = self.get_fr(uri);
        self.recv_sync(x)
    }

    // Performs sync POST
    #[inline]
    fn post<Q, R>(&mut self, uri: Uri, q: &Q, auth_opt: &Option<(String, String)>) -> ZRResult<R> where
        Q: serde::ser::Serialize,
        R: serde::de::DeserializeOwned {
        let mut req= Request::new(Method::Post, uri);
        let data =  serde_json::to_vec(q)?;
        let dlen = data.len();
        let body: Body = data.into();
        req.set_body(body);
        {
            let h = req.headers_mut();
            h.set(ContentType::json());
            h.set(ContentLength(dlen as u64));
            match auth_opt {
                &Some((ref user, ref pass)) => h.set(
                    Authorization(Basic { username: user.clone(), password: Some(pass.clone()) }
                    )),
                    _=> ()
            };
        }
        let x = self.req_fr(req);
        self.recv_sync(x)
    }
}

/// Http client
pub struct ZHttpClient {
    core: Core,
    client: Client<HttpConnector, Body>
}

impl ZHttpClient {
    pub fn new() -> ZRResult<ZHttpClient> {
        let c = Core::new()?;
        let h = c.handle();
        Ok(ZHttpClient { core: c, client: Client::new(&h)})
    }
}

impl CoreH for ZHttpClient {
    #[inline]
    fn core(&self) -> &Core { &self.core }
    #[inline]
    fn core_mut(&mut self) -> &mut Core { &mut self.core }
}

impl ZClient for ZHttpClient {
    #[inline]
    fn get_fr(&self, uri: Uri) -> FutureResponse {
        self.client.get(uri)
    }
    #[inline]
    fn req_fr(&self, req: Request<Body>) -> FutureResponse {
        self.client.request(req)
    }

}

/// Secure http client
pub struct ZHttpsClient {
    core: Core,
    client: Client<HttpsConnector<HttpConnector>, Body>
}

impl ZHttpsClient {
    pub fn new(n_threads: usize) -> ZRResult<ZHttpsClient> {
        let c = Core::new()?;
        let h = c.handle();
        let conn = HttpsConnector::new(n_threads, &h)?;
        Ok(ZHttpsClient { core: c, client: Client::configure().connector(conn).build(&h)})
    }
}

impl CoreH for ZHttpsClient {
    #[inline]
    fn core(&self) -> &Core { &self.core }
    #[inline]
    fn core_mut(&mut self) -> &mut Core { &mut self.core }
}

impl ZClient for ZHttpsClient {
    #[inline]
    fn get_fr(&self, uri: Uri) -> FutureResponse {
        self.client.get(uri)
    }
    #[inline]
    fn req_fr(&self, req: Request<Body>) -> FutureResponse {
        self.client.request(req)
    }
}

