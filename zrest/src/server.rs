use futures::{Future, Stream};
use futures::future;
//use tokio_core::reactor::Core;

use hyper;
use hyper::{Request, Response, Body, Chunk, StatusCode, Uri, Method};
use hyper::error::Error;
use hyper::server::NewService;
use hyper::header::{/*Authorization, Basic,*/ ContentType, ContentLength, Server};
use hyper::server::{Http, Service};
use hyper::mime;

use serde_json;
use serde_json::Value as JsValue;
use serde::ser::Serialize as Ser;
use serde::de::DeserializeOwned as Des;

use std::path::Path;
use std::io::Write;

use router::*;
use ::*;

//--------------------------------------------------------------------------------------------------

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

fn process_input<Q, R>(
    req: Request,
    it: InputType,
    hx: Box<Fn(Input<Q>) -> Box<Future<Item=Output<R>, Error=Error>>>)
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
            //TODO stop chunk (?)
                Box::new(future::ok(Input::Empty))
        };

    let output_i = input_f.and_then(move |i|
        //verbatim copy an input error; run any other input through hx
        match i {
            Input::Error(e) => Box::new(future::ok(Output::Error(e))),
            oth => hx(oth)
        }
    );

    Box::new(output_i)
}

//--------------------------------------------------------------------------------------------------
const SERVER_HEADER_VALUE: &'static str = concat!("zrest/", env!("CARGO_PKG_VERSION"));

#[inline]
fn resp_stub() -> Response {
    Response::new().with_header(Server::new(SERVER_HEADER_VALUE))
}

fn generate_output<R>(output_i: Box<Future<Item=Output<R>, Error=Error>>)
    -> Box<Future<Item=Response, Error=Error>>
    where
        R: Ser + 'static + From<ZRError>
{
     fn as_json<'r, T: Ser, R: Ser + 'r + From<ZRError>>(rv: &T) -> Response {
        match serde_json::to_vec(&rv) {
            Ok(b) => resp_stub()
                .with_header(ContentType::json())
                .with_header(ContentLength(b.len() as u64))
                .with_body(b),
            Err(e) =>   // encoding failed
                match serde_json::to_vec(&(R::from(ZRError::from(e)))) {
                    Ok(b) => resp_stub()
                        .with_status(StatusCode::InternalServerError)
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
            // TODO process Error
            _ =>
                resp_stub().with_status(StatusCode::NotImplemented)
        });

    Box::new(output_f)
}

//--------------------------------------------------------------------------------------------------

//pub type AsyncActionFn<'t, Q, R> = Box<Fn() -> AsyncPreResult<Q, R> + 't>;
pub type AsyncActionFn<'t, Q, R> = Box<Fn(Vec<Val>, Option<u64>) -> AsyncResult<Q, R> + 't>;
pub type AsyncResult<Q, R> = (InputType, Box<Fn(Input<Q>) -> Box<Future<Item=Output<R>, Error=Error>>>);

trait AsyncA<Q, R> where Q: Des, R:Ser {
    fn apply(&self, vars: Vec<Val>, content_length: Option<u64>)-> AsyncResult<Q, R>;
}

impl<'t, Q, R> AsyncA<Q, R> for AsyncActionFn<'t, Q, R> where Q: Des, R:Ser {
    fn apply(&self, vars: Vec<Val>, content_length: Option<u64>) -> AsyncResult<Q, R> {
        (*self)(vars, content_length)
    }
}

//--------------------------------------------------------------------------------------------------

fn exec_action<'t, Q, R>(
    f: &'t AsyncA<Q, R>,
    req: Request,
    vars: Vec<Val>
) -> Box<Future<Item=Response, Error=Error>>
where
    Q: Des + 'static,
    R: Ser + 'static + From<ZRError>
{
    let (it, hx) = f.apply(vars, req.headers().get::<ContentLength>().map(|h| **h));
    let o_t = process_input(req, it,hx);
    let result = generate_output(o_t);
    result
}

//--------------------------------------------------------------------------------------------------

pub fn success<'t, Q, R>() -> AsyncActionFn<'t, Q, R> where
    Q: Des + 'static,
    R: Ser + 'static //+ From<serde_json::Error>
{
    Box::new(|_, _| (
        InputType::Empty,
        Box::new(|_|
            Box::new(future::ok::<Output<R>, Error>(Output::Empty)))
    ))
}

/*
pub fn constant<'t, Q, R>(v: Output<R>) -> AsyncActionFn<'t, Q, R> where
    Q: Des + 'static,
    R: Ser + 'static, //+ From<serde_json::Error>
{
    Box::new(move |_, _| (
        InputType::Ignore,
        Box::new(move |_|
            Box::new(future::ok::<Output<R>, Error>(v))
        )
    ))
}
*/
//--------------------------------------------------------------------------------------------------

/// a very basic generic output wrapper
#[derive(Serialize)]
pub struct GOW<T> where T: Ser {
    ok: bool,
    data: Option<T>,
    message: Option<String>
}

impl<T> GOW<T> where T: Ser {
    pub fn ok(t: T) -> GOW<T> {
        GOW {
            ok: true,
            data: Some(t),
            message: None
        }
    }
}

impl<T> From<ZRError> for GOW<T> where T: Ser {
    fn from(e: ZRError) -> Self {
        GOW {
            ok: false,
            data: None,
            // TODO: make this up
            message: Some("<ZRError>".to_string())  //format!("{}", e)
        }
    }
}

//--------------------------------------------------------------------------------------------------
pub struct ZService<'t, Q, R> where Q: Des + 'static, R: Ser + 'static {
    rt: RTree<AsyncActionFn<'t, Q, R>>
}

impl<'t, Q, R> ZService<'t, Q, R> where Q: Des, R: Ser {
    pub fn new(router_table: Vec<(Vec<Condition>, AsyncActionFn<'t, Q, R>)>,
               failure_action: AsyncActionFn<'t, Q, R>) -> ZRResult<Self>
    {
        let mut b = Builder::new();
        for (cond, a) in router_table {
            b.mount(cond, a)
        }
        Ok(ZService { rt: RTree::build(b, failure_action)? })
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
        // TODO: fix unwrap
        let (rh, vars) = self.rt.run(req.uri().path()).unwrap();
        exec_action(rh, req, vars)
    }
}

/*
pub type ZServiceGen<'t, Q, R> = Fn() -> () where Q: Des, R: Ser  {
    fn generate() -> Vec<(Vec<Condition>, AsyncActionFn<'t, Q, R>)>;
    fn failure_action() -> AsyncActionFn<'t, Q, R>
}*/


impl From<ZRError> for JsValue {
    fn from(e: ZRError) -> Self {
        //TODO: make this up
        json!({"status":"error", "message": "<ZRERR>"})
    }
}

//type UntypedZServer = ZServer<'static, 'static, JsValue, JsValue>;

#[macro_export]
macro_rules! route(
    { $($cond:expr),* => $act:expr } => {
        (vec![$(Condition::from($cond)),*], $act)
    };
);



#[test]
fn test_server() {

    let addr = "127.0.0.1:3000".parse().unwrap();
    let server = Http::new().bind(&addr, || {
        let v = vec![
            route!("a" , "b" => success::<JsValue, JsValue>())
        ];

        Ok(ZService::new(v, success::<JsValue, JsValue>()).unwrap())
    }).unwrap();
    //server.run().unwrap();
}
