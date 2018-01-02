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

//pub type AsyncActionFn<'t, Q, R> = Box<Fn() -> AsyncPreResult<Q, R> + 't>;
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
/// error wrapper
#[derive(Serialize)]
pub struct EW {
    message: String,
    detail: String
}

/// a very basic generic output wrapper
#[derive(Serialize)]
pub struct GOW<T> where T: Ser {
    ok: bool,
    data: Option<T>,
    error: Option<EW>
}

impl<T> GOW<T> where T: Ser {
    pub fn ok(t: T) -> GOW<T> {
        GOW {
            ok: true,
            data: Some(t),
            error: None
        }
    }

    pub fn error(e: &ZRError) -> GOW<T> {
        GOW {
            ok: false,
            data: None,
            error: Some(EW { message: e.description().to_owned(), detail: format!("{}", e) })
        }
    }
}

impl<T> From<ZRError> for GOW<T> where T: Ser {
    fn from(e: ZRError) -> Self { GOW::error(&e) }
}

impl<'t, T> From<&'t ZRError> for GOW<T> where T: Ser {
    fn from(e: &'t ZRError) -> Self { GOW::error(e) }
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

//type UntypedZServer = ZServer<'static, 'static, JsValue, JsValue>;

#[macro_export]
macro_rules! to_new_service(
    { $z:expr } => { {
        let svc = std::sync::Arc::new($z);
        move || Ok(svc.clone())
    } }
);

#[macro_export]
macro_rules! const_action(
    { $c:expr } => { |_, _| (::server::InputType::Ignore, Box::new(|_, _| Box::new(::futures::future::ok($c)))) }
);

macro_rules! no_input_action(
    { $c:expr } => { |_, _| (::server::InputType::Ignore, Box::new(|vars, _| Box::new(::futures::future::ok($c(vars))))) }
);

#[macro_export]
macro_rules! empty_action(
    { } => { const_action!(::server::Output::Empty) }
);

#[macro_export]
macro_rules! error_action(
    { $s:expr } => { const_action!(::server::Output::Error(ZRError::EZR($s.to_owned()))) }
);
