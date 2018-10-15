use futures::{self, Future};
use http::uri::Scheme;
use hyper::{
    self,
    Request, Body, Uri,
    client::{Client, ResponseFuture, HttpConnector}
};
use hyper_tls::HttpsConnector;
use http::{self, request::Builder as RequestBuilder, method::Method, uri::{Parts, PathAndQuery}};
use auth;
use error::*;
use common::*;
use data_io::*;
use router::{ValRef, Condition, parse_uri_pattern, build_uri};

pub use data_io::{InputType, Input, Output};

pub struct UriPattern {
    base: Uri,
    p: Vec<Condition>
}

impl UriPattern {
    fn new(base: &str, pattern: &str) -> ZResult<UriPattern> {
        let base: Uri = base.parse()?;
        let p = parse_uri_pattern(pattern);
        Ok(UriPattern { base, p })
    }
    fn apply<'a>(&'a self, args: &'a [ValRef<'a>]) -> ZResult<Uri> {
        fn extend_path(pq: Option<PathAndQuery>, path_ext: &str) -> ZResult<PathAndQuery> {
            let mut p = pq.as_ref().map(|pq| pq.path()).unwrap_or_else(|| "/").to_owned();
            if !(p.ends_with('/') || path_ext.starts_with('/')) { p.push('/') }
            p += path_ext;
            pq.map(|pq| pq.query().map(|q| { p.push('?'); p += q }));
            Ok(p.parse()?)
        }

        let mut w = self.base.clone().into_parts();
        let pq = extend_path(w.path_and_query, &build_uri(&self.p, args)?)?;
        w.path_and_query = Some(pq);
        Ok(Uri::from_parts(w)?)
    }
}

#[test]
fn test_uri_pattern() {
    let p = UriPattern::new("https://www.google.com/a/b?p=q", "x1/$s/x2/$i").unwrap();
    let u = p.apply(&[ValRef::Str("Vx1"), ValRef::Int(15)]).unwrap();
    //println!("##### {}", u);
    assert_eq!(u.to_string(), "https://www.google.com/a/b/x1/Vx1/x2/15?p=q")
}

#[inline]
fn http_process_response<R>(
    rf: ResponseFuture,
    parse_as: InputType
) -> impl Future<Item=Input<R>, Error=ZError> + Send
    where R: Des + Send + 'static {
    rf
        .map_err(|err| err.into())
        .map(|r| parse_response(r, parse_as))
}

#[inline]
fn http_empty_body(mut request: RequestBuilder) -> Result<Request<Body>, ZError> {
    Ok(request.body(Body::empty())?)
}

enum Httpx {
    Http(Client<HttpConnector, Body>),
    Https(Client<HttpsConnector<HttpConnector>, Body>)
}

impl Httpx {
    fn new(uri: &Uri) -> Result<Httpx, ZError> {
        if Some(&Scheme::HTTPS) == uri.scheme_part() {
            Ok(HttpsConnector::new(1)
                .map(|connector|
                    Httpx::Https(Client::builder().build::<_, hyper::Body>(connector))
                )?)
        } else {
            Ok(Httpx::Http(Client::new()))
        }
    }

    fn request_raw(&self, r: Request<Body>) -> ResponseFuture {
        match self {
            Httpx::Http(c) => c.request(r),
            Httpx::Https(c) => c.request(r),
        }
    }
}

pub struct ZClient {
    endpoint: Httpx,
    auth: Box<Fn(&mut RequestBuilder) + Send>,
}


impl ZClient
{
    pub fn new(uri: &Uri, auth: Option<(String, String)>) -> Result<ZClient, ZError> {
        let mut a = auth::BasicAuth::new("zrest")?;
        for (u, p) in auth {
            a.add_user(&u, &p);
        }
        Httpx::new(uri).map(|p| ZClient { endpoint: p, auth: a.run_client() })
    }

    #[inline]
    fn create_request(&self, method: http::method::Method, uri: Uri) -> RequestBuilder {
        let mut r = RequestBuilder::new();
        r.method(method).uri(uri);
        (self.auth)(&mut r);
        r
    }

    pub fn do_with_no_input<R>(&self, method: Method, uri: Uri)
                            -> impl FnOnce(InputType) -> Box<dyn Future<Item=Input<R>, Error=ZError> + Send> + Send
        where R: Des + Send + 'static
    {
        let r = self.create_request(method, uri);
        let f = http_empty_body(r)
            .map(|r| self.endpoint.request_raw(r));
        |parse_as|
            Box::new(futures::future::result(f).and_then(|e|
                http_process_response(e, parse_as)
            ))
    }

    pub fn do_with_input<Q, R>(&self, method: Method, uri: Uri, q: Output<Q>)
                               -> impl FnOnce(InputType) -> Box<dyn Future<Item=Input<R>, Error=ZError> + Send> + Send
        where Q: Ser,
              R: Des + Send + 'static
    {
        let r = self.create_request(method, uri);
        let f = build_request(q, r)
            .map(|r| self.endpoint.request_raw(r));
        |parse_as|
            Box::new(futures::future::result(f).and_then(|e|
                http_process_response(e, parse_as)
            ))
    }

    pub fn get<R>(&self, uri: Uri)
                  -> impl FnOnce(InputType) -> Box<dyn Future<Item=Input<R>, Error=ZError> + Send> + Send
        where R: Des + Send + 'static
    {
        self.do_with_no_input(Method::GET, uri)
    }
    pub fn post<Q, R>(&self, uri: Uri, q: Output<Q>) -> impl FnOnce(InputType) -> Box<dyn Future<Item=Input<R>, Error=ZError> + Send> + Send
        where Q: Ser,
              R: Des + Send + 'static
    {
        self.do_with_input(Method::POST, uri, q)
    }
    pub fn put<Q, R>(&self, uri: Uri, q: Output<Q>) -> impl FnOnce(InputType) -> Box<dyn Future<Item=Input<R>, Error=ZError> + Send> + Send
        where Q: Ser,
              R: Des + Send + 'static
    {
        self.do_with_input(Method::PUT, uri, q)
    }}

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod client_tests {
    use client::*;
    use futures::{future, Future};
    use tokio::runtime::Runtime;

    fn f_wait<I>(f: impl FnOnce(InputType) -> Box<dyn Future<Item=Input<I>, Error=ZError> + Send> + Send) -> Result<I, ZError>
        where I: Des + Send + 'static
    {
        let g = f(InputType::Typed).and_then(|input|
            if let Input::Typed(i) = input {
                i
            } else {
                Box::new(future::err(rest_error!(other "invalid reply type")))
            }
        );
        Runtime::new().unwrap().block_on(g)
    }


    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct R {
        operation: String,
        expression: String,
        result: String
    }
    #[test]
    fn test_get_s() {
        let uri = "https://newton.now.sh/factor/x%5E2-1".parse().unwrap();
        let cl = ZClient::new(&uri, None).unwrap();
        let res = f_wait(cl.get::<R>(uri)).unwrap();
        assert_eq!(res , R {
            operation: "factor".to_string(),
            expression: "x^2-1".to_string(),
            result: "(x - 1) (x + 1)".to_string()
        });
        println!("{:?}", res);
    }


    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct PubReq {
        title: String,
        body: String,
        #[serde(rename="userId")]
        user_id: i32,
        id: i32
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct PubResp {
        title: String,
        body: String,
        #[serde(rename="userId")]
        user_id: i32,
        id: i32
    }

    #[test]
    fn test_post_s() {
        let uri = "http://jsonplaceholder.typicode.com/posts".parse().unwrap();
        let cl = ZClient::new(&uri, None).unwrap();
        let res = f_wait(cl.post::<PubReq, PubResp>(
            uri,
            Output::Typed(&PubReq { title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111, id: 1001 })
        )).unwrap();
        assert_eq!(res , PubResp {
            title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111,
            id: 1001
        });
        println!("{:?}", res);
    }

    #[test]
    fn test_post() {
        let uri = "http://jsonplaceholder.typicode.com/posts".parse().unwrap();
        let cl = ZClient::new(&uri, None).unwrap();
        let res = f_wait(cl.post::<PubReq, PubResp>(
            uri,
            Output::Typed(&PubReq { title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111, id: 1002 })
        )).unwrap();
        assert_eq!(res , PubResp {
            title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111,
            id: 1002
        });
        println!("{:?}", res);
    }

}
