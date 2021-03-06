use std::{
    fmt::Display,
    net::SocketAddr,
    sync::Arc
};
use tls_server;
use tokio_tcp::TcpListener;
use futures::{Future, Stream, future};
use hyper::{
    self,
    Body, Request, Response, Method,  //StatusCode,
    header::SERVER,
    server::Server,
    service::{service_fn, Service, NewService},
    rt
};
use http::response::Builder as ResponseBuilder;
use serde_json::{
    self,
    Value as JsValue
};
use serde;
use mime;

use common::*;
use auth::*;
use error::*;
use router::*;
use data_io::*;

pub enum ServerType {
    HTTP,
    HTTPS { cert: Vec<u8>, passwd: String }
}

impl ServerType {
    pub fn run_service<K>(self, addr: &SocketAddr, new_service: K) -> Result<(), ZError> where
        K: NewService<ReqBody=Body, ResBody=Body, Error=ZError> + Send + 'static,
        <<K as NewService>::Service as Service>::Future: Send,
        <K as NewService>::Future: Send,
        <K as NewService>::Service: Send,
        <K as NewService>::InitError: Display
    {
        match self {
            ServerType::HTTPS { cert, passwd } => {
                let tls_cx = tls_server::create_tls_cx(&cert, &passwd)?;
                let srv = TcpListener::bind(addr)?;
                let http_server = tls_server::TlsServer::create(srv, tls_cx, new_service);
                info!("(HTTPS) Listening on {}", addr);
                rt::run(http_server);
                Ok(())
            }
            ServerType::HTTP => {
                let http_server = Server::bind(addr)
                    .serve(new_service)
                    .map_err(|e| error!("Error creating service: {}", e))
                ;
                info!("Listening on {}", addr);
                rt::run(http_server);
                Ok(())
            }
        }
    }

    pub fn run_pipeline_factory(self, addr: &SocketAddr, handler: impl Fn() -> Pipeline + Send + 'static) -> Result<(), ZError>
    {
        let new_service=
            move || service_fn({
                let h = handler();
                move |req| {
                    info!("Incoming req:: {:?}", req);
                    h.apply(req, QAttr::new())
                }
            });
        self.run_service(addr, new_service)
    }

    pub fn run_transform(self, addr: &SocketAddr, pt: impl PipelineTransform) -> Result<(), ZError> {
        let h = move || pt.apply(Pipeline::default());
        self.run_pipeline_factory(addr, h)
    }
}

//--------------------------------------------------------------------------------------------------
const SERVER_HEADER_VALUE: &'static str = concat!("zrest/", env!("CARGO_PKG_VERSION"));

#[inline]
fn resp_stub() -> ResponseBuilder {
    let mut rv = Response::builder();
    rv.header(SERVER, SERVER_HEADER_VALUE);
    rv
}

pub struct AsyncActionFn<Cx> {
    h: Box<Fn(&Cx, Vec<Val>, Request<Body>) -> BFR + Send + Sync>
}

impl<Cx> AsyncActionFn<Cx> {
    fn new(a: impl Fn(&Cx, Vec<Val>, Request<Body>) -> BFR + Send + Sync + 'static) -> AsyncActionFn<Cx> {
        AsyncActionFn { h: Box::new(a) }
    }
    pub fn constant<O: Ser + Send + Sync + 'static>(o: O) -> AsyncActionFn<Cx> {
        Self::new(move |_, _, _| Box::new(future::result(build_json_response(&o, resp_stub()))))
    }
    pub fn error(text: &'static str) -> AsyncActionFn<Cx> {
        Self::new(move |_, _, _| Box::new(future::err(rest_error!(other "{}", text))))
    }

    /// asynchronous json-to-json
    pub fn async_jj<I, O, E, F, G>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> F + Send + Sync + 'static,
        F: FnOnce(I) -> G + Send + 'static,
        G: Future<Item=O, Error=ZError> + Send + 'static,
        Cx: Sync,
        I: Des + Send + 'static,
        O: Ser + Send + 'static
    {
        Self::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                let f = e(cx, vals);
                Box::new(
                    parse_as_json::<I>(r.into_body())
                        .and_then(move |v| f(v))
                        .and_then(|o: O| future::result(build_json_response(&o, resp_stub())))
                )
            }
        )
    }

    /// synchronous json-to-json
    pub fn sync_jj<I, O, E, F>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> F + Send + Sync + 'static,
        F: FnOnce(I) -> ZResult<O> + Send + 'static,
        Cx: Sync,
        I: Des + Send + 'static,
        O: Ser + Send + 'static
    {
        Self::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                let f = e(cx, vals);
                Box::new(
                    parse_as_json::<I>(r.into_body())
                        .and_then(move |v| f(v))
                        .and_then(|o: O| future::result(build_json_response(&o, resp_stub())))
                )
            }
        )
    }

    /// asynchronous json-to-any
    pub fn async_ja<I, O, E, F, G>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> F + Send + Sync + 'static,
        F: FnOnce(I) -> G + Send + 'static,
        G: Future<Item=Output<O>, Error=ZError> + Send + 'static,
        Cx: Sync,
        I: Des + Send + 'static,
        O: Ser + Send + 'static
    {
        Self::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                let f = e(cx, vals);
                Box::new(
                    parse_as_json::<I>(r.into_body())
                        .and_then(move |v| f(v))
                        .and_then(|o: Output<O>| future::result(build_response(o, resp_stub())))
                )
            }
        )
    }

    /// asynchronous any-to-any
    pub fn async<I, O, E, F, G>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> ZResult<(F, InputType)> + Send + Sync + 'static,
        F: FnOnce(Input<I>) -> G + Send + 'static,
        G: Future<Item=Output<O>, Error=ZError> + Send + 'static,
        Cx: Sync,
        I: Des + Send + 'static,
        O: Ser + Send + 'static
    {
        Self::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                match e(cx, vals) {
                    Ok((f, input_type)) =>
                        Box::new(
                            parse::<I>(r.into_body(), input_type)
                                .and_then(move |v| f(v))
                                .and_then(|o: Output<O>| future::result(build_response(o, resp_stub())))
                        ),
                    Err(e) =>
                        Box::new(future::err(e))
                }

            }
        )
    }
}

pub struct AAF<Cx, I, O> {
    _t_cx: std::marker::PhantomData<Cx>,
    _t_i: std::marker::PhantomData<I>,
    _t_o: std::marker::PhantomData<O>,
}

impl<Cx, I, O> AAF<Cx, I, O> where
    Cx: Sync,
    I: Des + Send + 'static,
    O: Ser + Send + 'static
{
    /// asynchronous json-to-json
    pub fn async_jj<E, F, G>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> F + Send + Sync + 'static,
        F: FnOnce(I) -> G + Send + 'static,
        G: Future<Item=O, Error=ZError> + Send + 'static
    {
        AsyncActionFn::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                let f = e(cx, vals);
                Box::new(
                    parse_as_json::<I>(r.into_body())
                        .and_then(move |v| f(v))
                        .and_then(|o: O| future::result(build_json_response(&o, resp_stub())))
                )
            }
        )
    }

    /// synchronous json-to-json
    pub fn sync_jj<E, F>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> F + Send + Sync + 'static,
        F: FnOnce(I) -> ZResult<O> + Send + 'static
    {
        AsyncActionFn::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                let f = e(cx, vals);
                Box::new(
                    parse_as_json::<I>(r.into_body())
                        .and_then(move |v| f(v))
                        .and_then(|o: O| future::result(build_json_response(&o, resp_stub())))
                )
            }
        )
    }

    /// asynchronous json-to-any
    pub fn async_ja<E, F, G>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> F + Send + Sync + 'static,
        F: FnOnce(I) -> G + Send + 'static,
        G: Future<Item=Output<O>, Error=ZError> + Send + 'static
    {
        AsyncActionFn::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                let f = e(cx, vals);
                Box::new(
                    parse_as_json::<I>(r.into_body())
                        .and_then(move |v| f(v))
                        .and_then(|o: Output<O>| future::result(build_response(o, resp_stub())))
                )
            }
        )
    }

    /// asynchronous any-to-any
    pub fn async<E, F, G>(e: E) -> AsyncActionFn<Cx> where
        E: Fn(&Cx, Vec<Val>) -> ZResult<(F, InputType)> + Send + Sync + 'static,
        F: FnOnce(Input<I>) -> G + Send + 'static,
        G: Future<Item=Output<O>, Error=ZError> + Send + 'static
    {
        AsyncActionFn::new(
            move |cx: &Cx, vals: Vec<Val>, r: Request<Body>| {
                match e(cx, vals) {
                    Ok((f, input_type)) =>
                        Box::new(
                            parse::<I>(r.into_body(), input_type)
                                .and_then(move |v| f(v))
                                .and_then(|o: Output<O>| future::result(build_response(o, resp_stub())))
                        ),
                    Err(e) =>
                        Box::new(future::err(e))
                }

            }
        )
    }
}

pub struct ZService<Cx> {
    rt: RTree<AsyncActionFn<Cx>>,
    cx: Cx
}

impl<Cx> ZService<Cx> {
    pub fn new(builder: Builder<AsyncActionFn<Cx>>,
               failure_action: AsyncActionFn<Cx>,
               cx: Cx)
               -> Result<Self, ZError>
    {
        RTree::build(builder, failure_action)
            .map(|rt| ZService { rt, cx })
    }

    fn handle(&self, req: Request<Body>, qattr: QAttr) -> BFR {
        let ct = extract_content_type_from_request(&req);

        let r = ct.and_then(|ct| {
            let mct = match (req.method(), ct.as_ref()) {
                (&Method::GET, None) => MCTR::Get,
                (&Method::POST, Some(ct)) => MCTR::Post(ct),
                (&Method::PUT, Some(ct)) => MCTR::Put(ct),
                (&Method::DELETE, None) => MCTR::Delete,
                _ => MCTR::Other
            };
            self.rt.run(req.uri().path(), mct)
        });

        match r {
            Ok((rh, vars)) => (rh.h)(&self.cx, vars, req),
            Err(e) => Box::new(future::err(e))
        }
    }

    pub fn run(self, addr: &SocketAddr, st: ServerType) -> Result<(), ZError> where
        Cx: Sync + Send + 'static
    {
        st.run_transform(addr, (self.transform(), FormatErrors))
    }

    pub fn transform(self) -> ZServiceTransform<Cx> {
        ZServiceTransform::new(self)
    }
}

pub struct ZServiceTransform<Cx> {
    s: Arc<ZService<Cx>>
}

impl<Cx> ZServiceTransform<Cx> {
    pub fn new(svc: ZService<Cx>) -> ZServiceTransform<Cx> {
        ZServiceTransform { s: Arc::new(svc) }
    }
}

impl<Cx> PipelineTransform for ZServiceTransform<Cx> where Cx: Send + Sync + 'static{
    fn apply(&self, input: Pipeline) -> Pipeline {
        let s = self.s.clone();
        Pipeline::new(move |req, qattr| s.handle(req, qattr))
    }
}

impl<Cx> Service for ZService<Cx>
{
    type ReqBody = Body;
    type ResBody = Body;
    type Error = ZError;
    type Future = Box<Future<Item=Response<Self::ResBody>, Error=Self::Error>>;

    fn call(&mut self, req: Request<Body>) -> Self::Future {
        self.handle(req, QAttr::new())
    }
}

/*
#[macro_export]
macro_rules! error_action(
    { $s:expr } => { AsyncActionFn::error(||EW::new_s($s.to_owned(), "".to_owned())) };
    { $s:expr, $d:expr } => { AsyncActionFn::error::<EW, _>(||EW::new_s($s.to_owned(), $d.to_owned())) };
);

macro_rules! z_action {
    { [$vt:ty] => ($o:ty, $e:ty) { $b:expr } } => { AsyncActionFn::v_r::<$vt, $o, $e, _>($b) };
    { [$vt:ty] $i:ty => ($o:ty, $e:ty) { $b:expr } } => { AsyncActionFn::vq_r::<$vt, $i, $o, $e, _>($b) };
}
*/

//--------------------------------------------------------------------------------------------------

#[cfg(test)]
mod server_tests {
    //use hyper::server::{Service, NewService};
    use futures::sync::oneshot::*;
    use futures::Future;
    use super::*;
    use client::*;
    use tokio::runtime::Runtime;

    fn f_wait<I>(f: impl Future<Item=Input<I>, Error=ZError> + Send + 'static) -> Result<I, ZError>
        where I: Des + Send + 'static
    {
        let g =
            f.and_then(|input|
                if let Input::Typed(i) = input {
                    Ok(i)
                } else {
                    Err(rest_error!(other "invalid reply type"))
                }
            );
        Runtime::new().unwrap().block_on(g)
    }

    #[test]
    fn test_server_raw() {
        extern crate env_logger;
        let _ = env_logger::try_init();

        fn convert_vars(vars: Vec<Val>) -> JsValue {
            vars.into_iter().map(|val| match val {
                Val::Str(s) => json!({"str" : s}),
                Val::Int(i) => json!({"int" : i}),
                Val::Float(f) => json!({"float" : f}),
            }).collect()
        }

        let (snd, recv) = channel::<()>();
        let mut t = ::std::thread::spawn(|| {

            let b = route_builder!(AsyncActionFn<()>,
                "v0/static" => AsyncActionFn::<()>::constant(json!({"static":"true", "message": "v0/static"}))
                //"v0/str/$/a" => AsyncActionFn::v_r::<String, JsValue, JsValue, _>(|s| Ok(json!({"op":"a", "type": "str", "vars": [{"str" : s}] }))),
                //"v0/str/$/a" => z_action!([String] => (JsValue, JsValue) { |s| Ok(json!({"op":"a", "type": "str", "vars": [{"str" : s}] })) }),
                //"v0/mixed/$i/$f/a" => AsyncActionFn::v_r::<(i64, f64), JsValue, JsValue, _ >(|(i, f)| Ok(json!({"op":"a", "type": "mixed", "vars": [{"int" : i}, {"float" : f}] })))
            );

            let svc = ZService::new(b, AsyncActionFn::error("routing failed"), ()).unwrap();

            let addr = "127.0.0.1:3000".parse().unwrap();
            svc.run(&addr, ServerType::HTTP).unwrap()
        });
        //t.join();

        let cl = ZClient::new(&"http://127.0.0.1:3000/v0/static".parse().unwrap(), None).unwrap();

        let res: JsValue = f_wait(cl.get("http://127.0.0.1:3000/v0/static".parse().unwrap(), typed)).unwrap();
        assert_eq!(res, json!({"static":"true", "message": "v0/static"}));
        //println!("SERVER: {:?}", res);

        //let res: JsValue = f_wait(cl.get("http://127.0.0.1:3000/v0/str/abcd/a".parse().unwrap(), typed)).unwrap();
        //assert_eq!(res, json!({"op":"a", "type": "str", "vars": [{"str" : "abcd"}] }));
        //println!("SERVER: {:?}", res);

        //let res: JsValue = f_wait(cl.get("http://127.0.0.1:3000/v0/mixed/10/0.25/a".parse().unwrap(), typed)).unwrap();
        //assert_eq!(res, json!({"op":"a", "type": "mixed", "vars": [{"int" : 10}, {"float" : 0.25}] }));
        //println!("SERVER: {:?}", res);

        //std::thread::sleep_ms(60000);
        snd.send(()).unwrap();
    }

    /*
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
    */
}



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