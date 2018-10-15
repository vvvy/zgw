use std::net::SocketAddr;
use tls_server;
use tokio_tcp::TcpListener;
use futures::{Future, Stream, future};
use hyper::{
    self,
    Body, Request, Response, //Method,  StatusCode,
    server::Server,
    service::service_fn,
    rt
};
use http::response::Builder as ResponseBuilder;
use serde_json;
use serde;
use mime;

use common::*;
use auth::*;
use error::*;

fn http_process_request<Q>(
    req: Request<Body>
) -> impl Future<Item=Q, Error=ZError> + Send
    where Q: serde::de::DeserializeOwned + Send {
    use std::str::FromStr;
    let m = req.headers()
        .get(hyper::header::CONTENT_TYPE)
        .map(|s| s.to_str().map(|x| mime::Mime::from_str(x)));
    let r = future::result(match m {
        Some(Ok(Ok(ref ct))) if ct.type_() == mime::APPLICATION && ct.subtype() == mime::JSON =>
            Ok(req),
        Some(Ok(Ok(ref ct))) =>
            Err(rest_error!(other "invalid content type `{}`", ct)),
        Some(Ok(Err(ect))) =>
            Err(ect.into()),
        Some(Err(ect)) =>
            Err(ect.into()),
        None =>
            Err(rest_error!(other "no content type found (application/json required)"))
    });
    r.and_then(|req|
        req.into_body().concat2().map_err(|err| err.into())
    )
        .and_then(|body|
            serde_json::from_slice(&body).map_err(|err| err.into())
        )
}

fn http_attach_response_data<R>(mut rsp: ResponseBuilder, r: &R)-> Result<Response<Body>, ZError>
    where R: serde::ser::Serialize
{
    rsp.header(hyper::header::CONTENT_TYPE, mime::APPLICATION_JSON.as_ref());
    let data =  serde_json::to_vec(r)?;
    //let dlen = data.len();
    //req.header(hyper::header::CONTENT_LENGTH, dlen as u64);
    let body: Body = data.into();
    let rv = rsp.body(body)?;
    Ok(rv)
}

pub enum ServerType {
    HTTP,
    HTTPS { cert: Vec<u8>, passwd: String }
}

impl ServerType {
    pub fn run_raw<K>(self, addr: SocketAddr, auth: BasicAuth, handler: impl Fn() -> K + Send + 'static) -> Result<(), ZError> where
        K: Fn(Request<Body>) -> BoxFut + Send + 'static
    {
        let srv = TcpListener::bind(&addr).map_err(|e| Into::<ZError>::into(e))?;
        let new_service=
            move || service_fn({
                let auth_handler = auth.clone().run_server(handler());
                move |req| {
                    info!("Incoming req:: {:?}", req);
                    auth_handler(req)
                }
            });
        info!("Listening on {}", addr);
        match self {
            ServerType::HTTPS { cert, passwd } => {
                let tls_cx = tls_server::create_tls_cx(&cert, &passwd)?;
                let http_server = tls_server::TlsServer::create(srv, tls_cx, new_service);
                rt::run(http_server);
                Ok(())
            }
            ServerType::HTTP => {
                let http_server = Server::bind(&addr)
                    .serve(new_service)
                    .map_err(|e| error!("Error creating service: {}", e))
                ;
                rt::run(http_server);
                Ok(())
            }
        }
    }

    pub fn run<K, L, M, Q, R>(self, addr: SocketAddr, auth: BasicAuth, handler: impl Fn() -> K + Send + 'static) -> Result<(), ZError> where
        K: Fn(&str) -> L + Send + 'static,
        L: Fn(Q) -> M + Send + 'static,
        M: Future<Item=R, Error=ZError> + Send + 'static,
        Q: serde::de::DeserializeOwned + Send + 'static,
        R: serde::ser::Serialize + Send
    {
        self.run_raw(addr, auth, move || {
            let k = handler();
            move |req: Request<Body>| {
                let l = k(req.uri().path());
                Box::new(http_process_request(req)
                    .and_then(move |q| l(q))
                    .and_then(|r: R| {
                        future::result(http_attach_response_data(ResponseBuilder::new(), &r))
                    }))
            }
        })
    }
}






//--------------------------------------------------------------------------------------------------
/*
#[cfg(test)]
mod server_tests {
    use hyper::server::Http;
    use hyper::server::{Service, NewService};
    use futures::sync::oneshot::*;
    use futures::Future;
    use super::*;

    /*
    #[test]
    fn test_server_raw() {
        fn convert_vars(vars: Vec<Val>) -> JsValue {
            vars.into_iter().map(|val| match val {
                Val::Str(s) => json!({"str" : s}),
                Val::Int(i) => json!({"int" : i}),
                Val::Float(f) => json!({"float" : f}),
            }).collect()
        }

        let (snd, recv) = channel::<()>();
        std::thread::spawn(|| {

            let b = route_builder!(
                AsyncActionFn<JsValue, JsValue>,
                "v0/static" => const_action!(Output::Typed(json!({"static":"true", "message": "v0/static"}))),
                "v0/str/$/a" => no_input_action!(|vars: Vec<Val>| Output::Typed(json!({"op":"a", "type": "str", "vars": convert_vars(vars) }))),
                "v0/mixed/$i/$f/a" => no_input_action!(|vars: Vec<Val>| Output::Typed(json!({"op":"a", "type": "mixed", "vars": convert_vars(vars) })))
            );

            let svc = ZService::new(b, Box::new(error_action!("routing failed"))).unwrap();

            let addr = "127.0.0.1:3000".parse().unwrap();
            let server = Http::new().bind(&addr, to_new_service!(svc)).unwrap();
            server.run_until(recv.map_err(|_canceled| ()))
        });

        let mut cl = ZHttpsClient::new(1).unwrap();

        let res = cl.get::<JsValue>("http://127.0.0.1:3000/v0/static".parse().unwrap()).unwrap();
        assert_eq!(res, json!({"static":"true", "message": "v0/static"}));
        //println!("SERVER: {:?}", res);

        let res = cl.get::<JsValue>("http://127.0.0.1:3000/v0/str/abcd/a".parse().unwrap()).unwrap();
        assert_eq!(res, json!({"op":"a", "type": "str", "vars": [{"str" : "abcd"}] }));
        //println!("SERVER: {:?}", res);

        let res = cl.get::<JsValue>("http://127.0.0.1:3000/v0/mixed/10/0.25/a".parse().unwrap()).unwrap();
        assert_eq!(res, json!({"op":"a", "type": "mixed", "vars": [{"int" : 10}, {"float" : 0.25}] }));
        //println!("SERVER: {:?}", res);

        //std::thread::sleep_ms(60000);
        snd.send(()).unwrap();
    }


    #[test]
    fn test_server_typed() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct TIO {
            s: String,
            i: i64
        }

        let (snd, recv) = channel::<()>();
        std::thread::spawn(|| {

            let b = route_builder!(
                AsyncActionFn<TIO, GOW<TIO>>,
                "v0/default" => const_action!(Output::Typed(GOW::ok(TIO { s: "default".to_owned(), i: 0 }))),
                "v0/mult/$i" => typed_input_action!(|vars: Vec<Val>, _| Output::Typed(GOW::ok(TIO { s: /*input.s +*/ "_multiplied".to_owned(), i: 1/*input.i*/ })))
            );

            let svc = ZService::new(b, Box::new(error_action!("routing failed"))).unwrap();

            let addr = "127.0.0.1:3001".parse().unwrap();
            let server = Http::new().bind(&addr, to_new_service!(svc)).unwrap();
            server.run_until(recv.map_err(|_canceled| ()))
        });

        let mut cl = ZHttpsClient::new(1).unwrap();
        let res = cl.get::<GOW<TIO>>("http://127.0.0.1:3001/v0/default".parse().unwrap()).unwrap();
        assert_eq!(res, GOW::ok(TIO { s: "default".to_owned(), i: 0 }));
        println!("### {:?}", res);

        snd.send(()).unwrap();
    }
    */
    #[test]
    fn test_server_raw() {
        fn convert_vars(vars: Vec<Val>) -> JsValue {
            vars.into_iter().map(|val| match val {
                Val::Str(s) => json!({"str" : s}),
                Val::Int(i) => json!({"int" : i}),
                Val::Float(f) => json!({"float" : f}),
            }).collect()
        }

        let (snd, recv) = channel::<()>();
        std::thread::spawn(|| {

            let b = route_builder!(AsyncActionFn,
                "v0/static" => AsyncActionFn::constant::<JsValue, JsValue, _>(|| json!({"static":"true", "message": "v0/static"})),
                //"v0/str/$/a" => AsyncActionFn::v_r::<String, JsValue, JsValue, _>(|s| Ok(json!({"op":"a", "type": "str", "vars": [{"str" : s}] }))),
                "v0/str/$/a" => z_action!([String] => (JsValue, JsValue) { |s| Ok(json!({"op":"a", "type": "str", "vars": [{"str" : s}] })) }),
                "v0/mixed/$i/$f/a" => AsyncActionFn::v_r::<(i64, f64), JsValue, JsValue, _ >(|(i, f)| Ok(json!({"op":"a", "type": "mixed", "vars": [{"int" : i}, {"float" : f}] })))
            );

            let svc = ZService::new(b, error_action!("routing failed")).unwrap();

            let addr = "127.0.0.1:3000".parse().unwrap();
            let server = Http::new().bind(&addr, to_new_service!(svc)).unwrap();
            server.run_until(recv.map_err(|_canceled| ()))
        });

        let mut cl = ZHttpsClient::new(1).unwrap();

        let res = cl.get::<JsValue>("http://127.0.0.1:3000/v0/static".parse().unwrap()).unwrap();
        assert_eq!(res, json!({"static":"true", "message": "v0/static"}));
        //println!("SERVER: {:?}", res);

        let res = cl.get::<JsValue>("http://127.0.0.1:3000/v0/str/abcd/a".parse().unwrap()).unwrap();
        assert_eq!(res, json!({"op":"a", "type": "str", "vars": [{"str" : "abcd"}] }));
        //println!("SERVER: {:?}", res);

        let res = cl.get::<JsValue>("http://127.0.0.1:3000/v0/mixed/10/0.25/a".parse().unwrap()).unwrap();
        assert_eq!(res, json!({"op":"a", "type": "mixed", "vars": [{"int" : 10}, {"float" : 0.25}] }));
        //println!("SERVER: {:?}", res);

        //std::thread::sleep_ms(60000);
        snd.send(()).unwrap();
    }

    #[test]
    fn test_server_custom() {
        #[derive(Serialize, Deserialize, Debug, PartialEq)]
        struct TIO {
            s: String,
            i: i64
        }

        let (snd, recv) = channel::<()>();
        std::thread::spawn(|| {
            let b = route_builder!(AsyncActionFn,
                "v0/default" => AsyncActionFn::constant::<OW<TIO>, EW, _>(|| OW::new(TIO { s: "default".to_owned(), i: 0 })),
                "v0/n/$i" => AsyncActionFn::v_r::<i64, OW<TIO>, EW, _>(|n: i64| Ok(OW::new(TIO { s: "n".to_owned(), i: n }))),
                "v0/nm/$i" => AsyncActionFn::vq_r::<i64, TIO, OW<TIO>, EW, _>(|n: i64, q: TIO| Ok(OW::new(TIO { s: q.s + "_nm", i: n * q.i })))
            );

            let svc = ZService::new(b, error_action!("routing failed")).unwrap();

            let addr = "127.0.0.1:3002".parse().unwrap();
            let server = Http::new().bind(&addr, to_new_service!(svc)).unwrap();
            server.run_until(recv.map_err(|_canceled| ()))
        });

        let mut cl = ZHttpsClient::new(1).unwrap();
        let res = cl.get::<GOW<TIO>>("http://127.0.0.1:3002/v0/default".parse().unwrap()).unwrap();
        assert_eq!(res, GOW::ok(TIO { s: "default".to_owned(), i: 0 }));
        println!("### {:?}", res);

        let res = cl.get::<GOW<TIO>>("http://127.0.0.1:3002/v0/n/33".parse().unwrap()).unwrap();
        assert_eq!(res, GOW::ok(TIO { s: "n".to_owned(), i: 33 }));
        println!("### {:?}", res);
        let res = cl.post::<TIO, GOW<TIO>>("http://127.0.0.1:3002/v0/nm/44".parse().unwrap(), &TIO { s: "abc".to_owned(), i: 2 }, &None).unwrap();
        assert_eq!(res, GOW::ok(TIO { s: "abc_nm".to_owned(), i: 88 }));
        println!("### {:?}", res);
        snd.send(()).unwrap();
    }

}

*/

//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------
//--------------------------------------------------------------------------------------------------



/*
use futures::{Future, Stream};
use futures::future;
//use tokio_core::reactor::Core;

use hyper;
use hyper::{Request, Response, Chunk, StatusCode, Method};
use hyper::error::Error;
use hyper::header::{/*Authorization, Basic,*/ ContentType, ContentLength, Server};
use hyper::server::{/*Http,*/ Service};
use hyper::mime;

use serde_json;
use serde_json::Value as JsValue;

use std::path::Path;
use std::io::Write;
use std::result::Result as StdResult;

use router::*;
use ::*;

//--------------------------------------------------------------------------------------------------
const SERVER_HEADER_VALUE: &'static str = concat!("zrest/", env!("CARGO_PKG_VERSION"));

#[inline]
fn resp_stub() -> Response {
    Response::new().with_header(Server::new(SERVER_HEADER_VALUE))
}

//--------------------------------------------------------------------------------------------------

pub struct AsyncActionFn {
    h: Box<Fn(Vec<Val>, Request) -> Box<Future<Item=Response, Error=Error>>>
}

pub struct ZService {
    rt: RTree<AsyncActionFn>
}

impl ZService {
    pub fn new(builder: Builder<AsyncActionFn>,
               failure_action: AsyncActionFn)
               -> Result<Self, std::io::Error>
    {
        RTree::build(builder, failure_action)
            .map(|t| ZService { rt: t })
            .map_err(|e|
                match e {
                    ZRError::EIO(io) => io,
                    e => std::io::Error::new(std::io::ErrorKind::Other, e)
                }
            )
    }
}

impl<'t> Service for ZService
{
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = Box<Future<Item=Self::Response, Error=Self::Error>>;

    fn call(&self, req: Request) -> Self::Future {
        let r = {
            let mct = {
                let ct = match req.headers().get::<ContentType>() {
                    Some(&ContentType(ref rmime)) => rmime,
                    None => &mime::APPLICATION_JSON
                };

                match req.method() {
                    &Method::Get => MCTR::Get,
                    &Method::Post => MCTR::Post(ct),
                    &Method::Put => MCTR::Put(ct),
                    &Method::Delete => MCTR::Delete,
                    _ => MCTR::Other
                }
            };
            self.rt.run(req.uri().path(), mct)
        };

        match r {
            Ok((rh, vars)) => (rh.h)(vars, req),
            Err(e) => Box::new(future::err(hyper::Error::Io(std::io::Error::new(std::io::ErrorKind::Other, e))))
        }
    }
}

//--------------------------------------------------------------------------------------------------
fn put_json_err<'r, E: Ser + From<ZRError>>(err: &'r E) -> Response {
    match serde_json::to_vec(err) {
        Ok(b) => resp_stub()
            .with_status(StatusCode::BadRequest)
            .with_header(ContentType::json())
            .with_header(ContentLength(b.len() as u64))
            .with_body(b),
        Err(e) => {
            // Should rarely or never happen
            let s = format!("Error encoding JSON serilalizer error: {}", e);
            resp_stub()
                .with_status(StatusCode::InternalServerError)
                .with_header(ContentType::plaintext())
                .with_header(ContentLength(s.len() as u64))
                .with_body(s)
        }
    }
}

fn put_json<'r, T: Ser, E: Ser + From<ZRError>>(rv: &'r T) -> Response {
    match serde_json::to_vec(rv) {
        Ok(b) => resp_stub()
            .with_header(ContentType::json())
            .with_header(ContentLength(b.len() as u64))
            .with_body(b),
        Err(e) =>   // encoding failed
            put_json_err(&(E::from(ZRError::from(e))))
    }
}

fn put_json_result<'r, T: Ser, E: Ser + From<ZRError>>(rv: &'r StdResult<T, E>) -> Response {
    match rv {
        &Ok(ref v) => put_json::<T,E>(v),
        &Err(ref e) => put_json_err(e)
    }
}

fn put_binary<E: Ser + From<ZRError>>(v: StdResult<(Chunk, mime::Mime), E>) -> Response {
    match v {
        Ok((b, ct)) => resp_stub()
            .with_header(ContentType(ct))
            .with_header(ContentLength(b.len() as u64))
            .with_body(b),
        Err(ref e) => put_json_err(e)
    }
}

fn get_json<Q: Des>(req: Request) -> Box<Future<Item=ZRResult<Q>, Error = Error>> {
    Box::new(
        req.body().concat2()
        .map(|k|
            serde_json::from_slice::<Q>(&*k).map_err(|e| ZRError::from(e))
        )
    )
}


pub trait FromOptVal {
    fn from(v: Option<Val>) -> Self;
}

impl FromOptVal for i64 {
    fn from(v: Option<Val>) -> Self { match v { Some(Val::Int(iv)) => iv, o => panic!("<i64 as FromOptVal>::from({:?})", o) } }
}
impl FromOptVal for f64 {
    fn from(v: Option<Val>) -> Self { match v { Some(Val::Float(fv)) => fv, o => panic!("<f64 as FromOptVal>::from({:?})", o) } }
}
impl FromOptVal for String {
    fn from(v: Option<Val>) -> Self { match v { Some(Val::Str(sv)) => sv, o => panic!("<String as FromOptVal>::from({:?})", o) } }
}

pub trait FromVecVal {
    fn from(_: Vec<Val>) -> Self;
}

impl FromVecVal for Vec<Val> {
    fn from(v: Vec<Val>) -> Self { v }
}

impl FromVecVal for i64 {
    fn from(v: Vec<Val>) -> Self {
        <i64 as FromOptVal>::from(v.into_iter().next())
    }
}

impl FromVecVal for f64 {
    fn from(v: Vec<Val>) -> Self {
        <f64 as FromOptVal>::from(v.into_iter().next())
    }
}

impl FromVecVal for String {
    fn from(v: Vec<Val>) -> Self {
        <String as FromOptVal>::from(v.into_iter().next())
    }
}

macro_rules! vec_val {
    { [$($t:ty),+], [$($v:ident),+] } => {
        impl FromVecVal for ($($t),+) {
            fn from(v: Vec<Val>) -> Self {
                let mut i = v.into_iter();
                $(
                    let $v = <$t as FromOptVal>::from(i.next());
                )+
                ( $($v),+ )
            }
        }
    };
    { $t1:ty, $t2:ty, $t3:ty, $t4:ty } => { vec_val! { [$t1, $t2, $t3, $t4], [r1, r2, r3, r4] } };
    { $t1:ty, $t2:ty, $t3:ty } => { vec_val! { [$t1, $t2, $t3], [r1, r2, r3] } };
    { $t1:ty, $t2:ty } => { vec_val! { [$t1, $t2], [r1, r2] } };
}

macro_rules! vec_val_table {
    { $( { $($t:ty),+ } ),+ } => { $(vec_val! { $($t),+ })+ }
}

vec_val_table! {
    {i64, i64},
    {i64, f64},
    {i64, String},
    {f64, i64},
    {f64, f64},
    {f64, String},
    {String, i64},
    {String, f64},
    {String, String},

    {String, i64, i64},
    {String, i64, f64},
    {String, i64, String},
    {String, f64, i64},
    {String, f64, f64},
    {String, f64, String},
    {String, String, i64},
    {String, String, f64},
    {String, String, String},

    {i64, i64, i64},
    {i64, i64, f64},
    {i64, i64, String},
    {i64, f64, i64},
    {i64, f64, f64},
    {i64, f64, String},
    {i64, String, i64},
    {i64, String, f64},
    {i64, String, String},

    {f64, i64, i64},
    {f64, i64, f64},
    {f64, i64, String},
    {f64, f64, i64},
    {f64, f64, f64},
    {f64, f64, String},
    {f64, String, i64},
    {f64, String, f64},
    {f64, String, String}
}


macro_rules! bf {
    { ok $e:expr } => { Box::new(::futures::future::ok($e)) };
    { err $e:expr } => { Box::new(::futures::future::err($e)) };
}



impl AsyncActionFn {
    pub fn constant<R, E, F>(f: F) -> AsyncActionFn where
        R: 'static + Ser,
        E: 'static + Ser + From<ZRError>,
        F: 'static + Fn() -> R
    {
        AsyncActionFn { h: Box::new(move |_, _| bf!(ok put_json::<R, E>(&f()))) }
    }

    pub fn error<E, F>(f: F) -> AsyncActionFn where
        E: 'static + Ser + From<ZRError>,
        F: 'static + Fn() -> E
    {
        AsyncActionFn { h: Box::new(move |_, _| bf!(ok put_json_err(&f()))) }
    }

    pub fn v_r<V, R, E, F>(f: F) -> AsyncActionFn where
        V: 'static + FromVecVal,
        R: 'static + Ser,
        E: 'static + Ser + From<ZRError>,
        F: 'static + Fn(V) -> StdResult<R, E>
    {
        AsyncActionFn { h: Box::new(move |vals, _| bf!(ok put_json_result(&f(V::from(vals))))) }
    }

    pub fn q_r<Q, R, E, F>(f: F) -> AsyncActionFn where
        Q: 'static + Des,
        R: 'static + Ser,
        E: 'static + Ser + From<ZRError>,
        F: 'static + Fn(Q) -> StdResult<R, E>
    {
        let rf = std::sync::Arc::new(f);
        AsyncActionFn { h: Box::new(move |_, req|
            Box::new({
                let g = rf.clone();
                get_json(req).map(move |r| match r {
                    Ok(q) => put_json_result(&g(q)),
                    Err(e) => put_json_err(&E::from(e))
                })
            })
        )}
    }

    pub fn vq_r<V, Q, R, E, F>(f: F) -> AsyncActionFn where
        V: 'static + FromVecVal,
        Q: 'static + Des,
        R: 'static + Ser,
        E: 'static + Ser + From<ZRError>,
        F: 'static + Fn(V, Q) -> StdResult<R, E>
    {
        let rf = std::sync::Arc::new(f);
        AsyncActionFn {
            h: Box::new(move |vals, req|
                Box::new({
                    let g = rf.clone();
                    get_json(req).map(move |r| match r {
                        Ok(q) => put_json_result(&g(V::from(vals), q)),
                        Err(e) => put_json_err(&E::from(e))
                    })
                })
            )
        }
    }

    pub fn v_binary<V, E, F>(f: F) -> AsyncActionFn where
        V: 'static + FromVecVal,
        E: 'static + Ser + From<ZRError>,
        F: 'static + Fn(V) -> StdResult<(Chunk, mime::Mime), E>
    {
        AsyncActionFn {
            h: Box::new(move |vals, _|
                bf!(ok put_binary(f(V::from(vals))))
            )
        }
    }
}

//--------------------------------------------------------------------------------------------------
impl From<ZRError> for JsValue {
    fn from(e: ZRError) -> Self {
        json!({
            "ok": false,
            "error": {
                "message": e.description().to_owned(),
                "detail": format!("{}", e)
            }
        })
    }
}



#[macro_export]
macro_rules! to_new_service(
    { $z:expr } => { {
        let svc = std::sync::Arc::new($z);
        move || Ok(svc.clone())
    } }
);

#[macro_export]
macro_rules! error_action(
    { $s:expr } => { AsyncActionFn::error::<EW, _>(||EW::new_s($s.to_owned(), "".to_owned())) };
    { $s:expr, $d:expr } => { AsyncActionFn::error::<EW, _>(||EW::new_s($s.to_owned(), $d.to_owned())) };
);

macro_rules! z_action {
    { [$vt:ty] => ($o:ty, $e:ty) { $b:expr } } => { AsyncActionFn::v_r::<$vt, $o, $e, _>($b) };
    { [$vt:ty] $i:ty => ($o:ty, $e:ty) { $b:expr } } => { AsyncActionFn::vq_r::<$vt, $i, $o, $e, _>($b) };
}

#[macro_export]
macro_rules! z_route_builder {
    { $($pat:expr => $act:tt),+ } => { {
        let mut b = ::router::Builder::<$t>::new();
        $(b.mount(::router::parse_uri_pattern($pat), $act);)+
        b
        } }
}

*/
//==================================================================================================
/*

pub enum InputType {
    /// Input MUST be empty
    Empty,
    /// Input as serde_json::Value
    Raw,
    /// Input formatted as Q
    Typed,
    /// Input is a raw in-memory buffer, of up to t length
    Memory(usize),
    /// Input is a temp file, of up to t length
    File(Box<Path>, usize),
    /// Ignore whatever has been sent
    Ignore
}

pub enum Input<Q: Des> {
    Empty,
    Raw(JsValue),
    Typed(Q),
    Memory(Chunk),
    File(Box<Path>),
    Error(ZRError)
}

pub enum Output<R:Ser> {
    Empty,
    Raw(JsValue),
    Typed(R),
    Memory(Vec<u8>, mime::Mime),
    File(Box<Path>, mime::Mime),
    Error(ZRError)
}
//--------------------------------------------------------------------------------------------------
type OutputGenerator<Q, R> = Box<Fn(Vec<Val>, Input<Q>) -> Box<Future<Item=Output<R>, Error=Error>>>;

fn process_input<Q, R>(
    req: Request,
    it: InputType,
    vars: Vec<Val>,
    hx: OutputGenerator<Q,R>)
    -> Box<Future<Item=Output<R>, Error=Error>>
    where
        Q: Des + 'static,
        R: Ser + 'static + From<ZRError>,
{
    use std::fs::File;

    let input_f: Box<Future<Item=Input<Q>, Error=Error>> =
        match it {
            InputType::Empty =>
            //TODO ensure input is empty
                Box::new(futures::future::ok(Input::Empty)),
            InputType::Memory(max_len) =>
                Box::new(
                    req
                        .body()
                        .concat2()
                        // TODO check for max_len
                        .map(|k| Input::Memory(k))
                ),
            InputType::Raw =>
                Box::new(
                    req
                        .body()
                        .concat2()
                        .map(|k|
                            serde_json::from_slice::<JsValue>(&*k)
                                .map(|q| Input::Raw(q))
                                .unwrap_or_else(|e| Input::Error(ZRError::from(e)))
                        )
                ),
            InputType::Typed =>
                Box::new(
                    req
                        .body()
                        .concat2()
                        .map(|k|
                            serde_json::from_slice::<Q>(&*k)
                                .map(|q| Input::Typed(q))
                                .unwrap_or_else(|e| Input::Error(ZRError::from(e)))
                        )
                ),
            InputType::File(path, max_size) =>
                Box::new(
                    req
                        .body()
                        // TODO check for max_len
                        .fold(
                            File::create(&path),
                            |rr, k|
                                rr.map(|mut w| w
                                    .write(&*k)
                                    .map(|_| w)
                                ),
                        ).map(|r|
                        r.map(|_| Input::File(path))
                            .map_err(|e| hyper::error::Error::Io(e))
                    ).flatten()
                ),
            InputType::Ignore =>
                Box::new(future::ok(Input::Empty))
        };

    let output_i = input_f.and_then(move |i|
        //verbatim copy an input error; run any other input through hx
        match i {
            Input::Error(e) => Box::new(future::ok(Output::Error(e))),
            oth => hx(vars, oth)
        }
    );

    Box::new(output_i)
}

fn generate_output<R>(output_i: Box<Future<Item=Output<R>, Error=Error>>)
                      -> Box<Future<Item=Response, Error=Error>>
    where
        R: Ser + 'static + From<ZRError>
{
    fn as_json_err<'r, R: Ser + 'r + From<ZRError>>(err: ZRError) -> Response {
        match serde_json::to_vec(&(R::from(err))) {
            Ok(b) => resp_stub()
                .with_status(StatusCode::BadRequest)
                .with_header(ContentType::json())
                .with_header(ContentLength(b.len() as u64))
                .with_body(b),
            Err(e) => {
                // Should rarely or never happen
                let s = format!("Error encoding JSON serilalizer error: {}", e);
                resp_stub()
                    .with_status(StatusCode::InternalServerError)
                    .with_header(ContentType::plaintext())
                    .with_header(ContentLength(s.len() as u64))
                    .with_body(s)
            }
        }
    }

    fn as_json<'r, T: Ser, R: Ser + 'r + From<ZRError>>(rv: &T) -> Response {
        match serde_json::to_vec(&rv) {
            Ok(b) => resp_stub()
                .with_header(ContentType::json())
                .with_header(ContentLength(b.len() as u64))
                .with_body(b),
            Err(e) =>   // encoding failed
                as_json_err::<R>(ZRError::from(e))
        }
    }

    let output_f =
        output_i.map(|o| match o {
            Output::Empty =>
                resp_stub(),
            Output::Raw(rv) =>
                as_json::<JsValue, R>(&rv),
            Output::Typed(rv) =>
                as_json::<R, R>(&rv),
            Output::Memory(b, ct) =>
                resp_stub()
                    .with_header(ContentType(ct))
                    .with_header(ContentLength(b.len() as u64))
                    .with_body(b),
            Output::Error(e) =>
                as_json_err::<R>(e),
            //TODO file
            _ =>
                resp_stub().with_status(StatusCode::NotImplemented)
        });

    Box::new(output_f)
}

//--------------------------------------------------------------------------------------------------

pub type AsyncActionFn<'t, Q, R> = Box<Fn(&Vec<Val>, Option<u64>) -> AsyncResult<Q, R> + 't>;
pub type AsyncResult<Q, R> = (InputType, OutputGenerator<Q, R>);

trait AsyncAction<Q, R> where Q: Des, R:Ser {
    fn apply(&self, vars: &Vec<Val>, content_length: Option<u64>)-> AsyncResult<Q, R>;
}

impl<'t, Q, R> AsyncAction<Q, R> for AsyncActionFn<'t, Q, R> where Q: Des, R:Ser {
    fn apply(&self, vars: &Vec<Val>, content_length: Option<u64>) -> AsyncResult<Q, R> {
        (*self)(vars, content_length)
    }
}

//--------------------------------------------------------------------------------------------------

fn exec_action<'t, Q, R>(
    f: &'t AsyncAction<Q, R>,
    req: Request,
    vars: Vec<Val>
) -> Box<Future<Item=Response, Error=Error>>
    where
        Q: Des + 'static,
        R: Ser + 'static + From<ZRError>
{
    let (it, hx) = f.apply(&vars, req.headers().get::<ContentLength>().map(|h| **h));
    let o_t = process_input(req, it,vars, hx);
    let result = generate_output(o_t);
    result
}


//--------------------------------------------------------------------------------------------------
pub struct ZService<'t, Q, R> where Q: Des + 'static, R: Ser + 'static {
    rt: RTree<AsyncActionFn<'t, Q, R>>
}

impl<'t, Q, R> ZService<'t, Q, R> where Q: Des, R: Ser + From<ZRError> {
    pub fn new(builder: Builder<AsyncActionFn<'t, Q, R>>,
               failure_action: AsyncActionFn<'t, Q, R>)
               -> Result<Self, std::io::Error>
    {
        RTree::build(builder, failure_action)
            .map(|t| ZService { rt: t })
            .map_err(|e|
                match e {
                    ZRError::EIO(io) => io,
                    e => std::io::Error::new(std::io::ErrorKind::Other, e)
                }
            )
    }
}

impl<'t, Q, R> Service for ZService<'t, Q, R> where
    Q: Des + 'static,
    R: Ser + 'static + From<ZRError>
{
    type Request = Request;
    type Response = Response;
    type Error = Error;
    type Future = Box<Future<Item=Self::Response, Error=Self::Error>>;

    fn call(&self, req: Request) -> Self::Future {
        let r = {
            let mct = {
                let ct = match req.headers().get::<ContentType>() {
                    Some(&ContentType(ref rmime)) => rmime,
                    None => &mime::APPLICATION_JSON
                };

                match req.method() {
                    &Method::Get => MCTR::Get,
                    &Method::Post => MCTR::Post(ct),
                    &Method::Put => MCTR::Put(ct),
                    &Method::Delete => MCTR::Delete,
                    _ => MCTR::Other
                }
            };
            self.rt.run(req.uri().path(), mct)
        };

        match r {
            Ok((rh, vars)) => exec_action(rh, req, vars),
            Err(e) => generate_output::<R>(Box::new(future::ok(Output::Error(e))))
        }
    }
}



//type UntypedZServer = ZServer<'static, 'static, JsValue, JsValue>;
#[macro_export]
macro_rules! const_action(
    { $c:expr } => { |_, _| (::server::InputType::Ignore, Box::new(|_, _| Box::new(::futures::future::ok($c)))) }
);

macro_rules! no_input_action(
    { $c:expr } => { |_, _| (::server::InputType::Ignore, Box::new(|vars, _| Box::new(::futures::future::ok($c(vars))))) }
);

macro_rules! typed_input_action(
    { $c:expr } => { |_, _| (::server::InputType::Typed, Box::new(|vars, ti| Box::new(::futures::future::ok($c(vars, ti))))) }
);


#[macro_export]
macro_rules! empty_action(
    { } => { const_action!(::server::Output::Empty) }
);

#[macro_export]
macro_rules! error_action(
    { $s:expr } => { const_action!(::server::Output::Error(ZRError::EZR($s.to_owned()))) }
);

*/