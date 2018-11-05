use common::*;
use error::*;
use futures::{future, Future, Stream};
use hyper::{
    Response, Request, Body, Chunk,
    header::{CONTENT_TYPE, CONTENT_LENGTH, HeaderValue}
};
use http::{
    response::Builder as ResponseBuilder,
    request::Builder as RequestBuilder
};
use mime::{Mime, APPLICATION_JSON, APPLICATION, JSON};
use serde_json::{
    self,
    Value as JsValue
};
use std::str::FromStr;

//--------------------------------------------------------------------------------------------------

fn parse_content_type(content_type: Option<&HeaderValue>) -> ZResult<Option<Mime>> {
    match content_type {
        None =>
            Ok(None),
        Some(s) =>
            s.to_str()
                .map_err(|e| e.into())
                .and_then(|x| Mime::from_str(x).map_err(|e| e.into()))
                .map(|x| Some(x))
    }
}

pub fn extract_content_type_from_request<B>(req: &Request<B>) -> ZResult<Option<Mime>> {
    parse_content_type(req.headers().get(CONTENT_TYPE))
}

pub fn extract_content_type_from_response<B>(req: &Response<B>) -> ZResult<Option<Mime>> {
    parse_content_type(req.headers().get(CONTENT_TYPE))
}

fn parse_content_length(content_length: Option<&HeaderValue>) -> ZResult<Option<u64>> {
    match content_length {
        None =>
            Ok(None),
        Some(s) =>
            s.to_str()
                .map_err(|e| e.into())
                .and_then(|x| x.parse().map_err(|e| rest_error!(other "Unable to parse contents of Content-Length: {}", e)))
                .map(|x| Some(x))
    }
}

//--------------------------------------------------------------------------------------------------

fn build_json<O>(o: &O) -> ZResult<Body> where O: Ser {
    Ok(serde_json::to_vec(o)?.into())
}

pub fn build_json_request<O>(o: &O, mut r: RequestBuilder) -> ZResult<Request<Body>> where O: Ser {
    build_json(o).and_then(|b|{
        r.header(CONTENT_TYPE, APPLICATION_JSON.as_ref());
        Ok(r.body(b)?)
    })
}

pub fn build_json_response<O>(o: &O, mut r: ResponseBuilder) -> ZResult<Response<Body>> where O: Ser {
    build_json(o).and_then(|b|{
        r.header(CONTENT_TYPE, APPLICATION_JSON.as_ref());
        Ok(r.body(b)?)
    })
}

pub fn parse_as_json<I>(b: Body) -> impl Future<Item=I, Error=ZError> where
    I: Des + Send + 'static
{
    b
        .concat2()
        .map_err(|err| err.into())
        .and_then(|body|
            serde_json::from_slice(&body).map_err(|err| err.into())
        )
}


//----------------------------------------------------------------------------------------------------------------------

#[derive(Copy, Clone)]
pub enum InputType {
    /// Input MUST be empty
    Empty,
    /// Input as serde_json::Value
    Raw,
    /// Input formatted as Q
    Typed,
    /// Input as stream
    Stream,
    /*
    /// Input is a raw in-memory buffer, of up to t length
    Memory(usize),
    /// Input is a temp file, of up to t length
    File(Box<Path>, usize),
    */
    /// Discard whatever has been sent
    Discard
}

pub enum Input<I> {
    Empty,
    Raw(JsValue),
    Typed(I),
    Stream(Box<dyn Stream<Item=Chunk, Error=ZError> + Send>),
    //Memory(Chunk),
    //File(Box<Path>),
    Error(ZError)
}

impl<I: Des> From<ZError> for Input<I> {
    fn from(e: ZError) -> Self { Input::Error(e) }
}


type BIF<I> = Box<dyn Future<Item=Input<I>, Error=ZError> + Send>;


pub enum Output<O> {
    Empty,
    Raw(JsValue),
    Typed(O),
    Stream(Box<dyn Stream<Item=Chunk, Error=ZError> + Send>, Mime),
    //Memory(Vec<u8>, mime::Mime),
    //File(Box<Path>, mime::Mime),
    Error(ZError)
}

fn build<O>(o: Output<O>) -> ZResult<(Option<Mime>, Body)> where O: Ser {
    match o {
        Output::Empty =>
            Ok((None, Body::empty())),
        Output::Raw(js_value) =>
            Ok((Some(APPLICATION_JSON), serde_json::to_vec(&js_value)?.into())),
        Output::Typed(o) =>
            Ok((Some(APPLICATION_JSON), serde_json::to_vec(&o)?.into())),
        Output::Stream(s, mime) =>
            Ok((Some(mime), Body::wrap_stream(s.map_err(|e| Box::new(e))))),
        Output::Error(e) =>
            Err(e)
    }
}

pub fn build_request<O>(o: Output<O>, mut r: RequestBuilder) -> ZResult<Request<Body>> where O: Ser {
    build(o).and_then(|(om, b)|{
        om.map(|m| r.header(CONTENT_TYPE, m.as_ref()));
        Ok(r.body(b)?)
    })
}

pub fn build_response<O>(o: Output<O>, mut r: ResponseBuilder) -> ZResult<Response<Body>> where O: Ser {
    build(o).and_then(|(om, b)|{
        om.map(|m| r.header(CONTENT_TYPE, m.as_ref()));
        Ok(r.body(b)?)
    })
}

//--------------------------------------------------------------------------------------------------

pub fn parse<I>(b: Body, parse_as: InputType) -> BIF<I> where
    I: Des + Send +  'static {
    use hyper::body::Payload;
    match parse_as {
        InputType::Empty =>
            if b.is_end_stream() {
                Box::new(future::ok(Input::Empty))
            } else {
                Box::new(future::err(rest_error!(other "Non-empty body where empty is expected")))
            }
        InputType::Raw => Box::new(parse_as_json( b).map(|v| Input::Raw(v))),
        InputType::Typed => Box::new(parse_as_json( b).map(|v| Input::Typed(v))),
        InputType::Stream => Box::new(future::ok(Input::Stream(Box::new(b.map_err(|e| e.into()))))),
        InputType::Discard => Box::new(future::ok(Input::Empty))
    }
}


fn parser<I>(prefetch: impl FnOnce(Option<&Mime>, Option<u64>) -> InputType)
    -> impl FnOnce(Result<Option<Mime>, ZError>, Result<Option<u64>, ZError>, Body) -> BIF<I>
    where I: Des + Send + 'static {
    |content_type, content_length, body| {
        match (content_type, content_length) {
            (Ok(content_type), Ok(content_length)) => {
                let input_type = prefetch(content_type.as_ref(), content_length);
                match (input_type, content_type) {
                    (InputType::Typed, Some(ref ct)) | (InputType::Raw, Some(ref ct)) if ct.type_() == APPLICATION && ct.subtype() == JSON =>
                        parse(body, input_type),
                    (InputType::Typed, Some(ref ct)) | (InputType::Raw, Some(ref ct)) =>
                        Box::new(future::err(rest_error!(other "invalid content type `{}`", ct))),
                    _ =>
                        parse(body, input_type)
                }
            },
            (Err(e), _) | (_, Err(e)) =>
                Box::new(future::err(e))
        }
    }
}

/// client-side response parser
pub fn response_parser<I>(prefetch: impl FnOnce(Option<&Mime>, Option<u64>) -> InputType)
    -> impl FnOnce(Response<Body>) -> BIF<I> where
    I: Des + Send + 'static
{
    let p = parser(prefetch);
    move |r: Response<Body>| {
        let content_type = extract_content_type_from_response(&r);
        let content_length = parse_content_length(r.headers().get(CONTENT_LENGTH));
        let body = r.into_body();
        p(content_type, content_length, body)
    }
}

/*
/// server-side request parser
pub fn request_parser<I>(prefetch: impl FnOnce(Option<&Mime>, Option<u64>) -> InputType)
                          -> impl FnOnce(Request<Body>) -> BIF<I> where
    I: Des + Send + 'static
{
    let p = parser(prefetch);
    move |r: Request<Body>| {
        let content_type = extract_content_type_from_request(&r);
        let content_length = parse_content_length(r.headers().get(CONTENT_LENGTH));
        let body = r.into_body();
        p(content_type, content_length, body)
    }
}
*/