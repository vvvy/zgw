use common::*;
use error::*;
use futures::{Future, Stream, future};
use hyper::{
    Response, Request, Body, Chunk,
    header::{CONTENT_TYPE, HeaderValue},
    HeaderMap
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

pub enum Input<I: Des> {
    Empty,
    Raw(Box<Future<Item=JsValue, Error=ZError> + Send>),
    Typed(Box<dyn Future<Item=I, Error=ZError> + Send>),
    Stream(Box<dyn Stream<Item=Chunk, Error=ZError> + Send>),
    //Memory(Chunk),
    //File(Box<Path>),
    Error(ZError)
}

pub enum Output<'v, O: Ser + 'v> {
    Empty,
    Raw(&'v JsValue),
    Typed(&'v O),
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

fn require_app_json_ct(hm: &HeaderMap<HeaderValue>) -> ZResult<()> {
    let m: Option<ZResult<Mime>>= hm.get(CONTENT_TYPE).map(|s|
        s.to_str()
            .map_err(|e| e.into())
            .and_then(|x| Mime::from_str(x).map_err(|e| e.into()))
    );
    match m {
        Some(Ok(ref ct)) if ct.type_() == APPLICATION && ct.subtype() == JSON =>
            Ok(()),
        Some(Ok(ct)) =>
            Err(rest_error!(other "invalid content type `{}`", ct)),
        Some(Err(ect)) =>
            Err(ect.into()),
        None =>
            Err(rest_error!(other "no content type found (application/json required)"))
    }
}

fn parse_as_json<I>(hm: &HeaderMap<HeaderValue>, b: Body, typed: bool) -> Input<I> where
    I: Des + Send + 'static {
    let y =
        future::result(require_app_json_ct(hm)).and_then(|_|
            b.concat2().map_err(|err| err.into())
        );
    if typed {
        Input::Typed(Box::new(y.and_then(|body| serde_json::from_slice(&body).map_err(|err| err.into()))))
    } else {
        Input::Raw(Box::new(y.and_then(|body| serde_json::from_slice(&body).map_err(|err| err.into()))))
    }
}


pub fn parse<I>(hm: &HeaderMap<HeaderValue>, b: Body, parse_as: InputType) -> Input<I> where
    I: Des + Send +  'static {
    use hyper::body::Payload;
    match parse_as {
        InputType::Empty =>
            if b.is_end_stream() {
                Input::Empty
            } else {
                Input::Error(rest_error!(other "Non-empty body where empty is expected"))
            }
        InputType::Raw => parse_as_json(hm, b, false),
        InputType::Typed => parse_as_json(hm, b, true),
        InputType::Stream => Input::Stream(Box::new(b.map_err(|e| e.into()))),
        InputType::Discard => Input::Empty
    }
}

pub fn parse_request<I>(r: Request<Body>, parse_as: InputType) -> Input<I> where I: Des + Send + 'static {
    let (parts, body) = r.into_parts();
    parse(&parts.headers, body, parse_as)
}
pub fn parse_response<I>(r: Response<Body>, parse_as: InputType) -> Input<I> where I: Des + Send + 'static {
    let (parts, body) = r.into_parts();
    parse(&parts.headers, body, parse_as)
}

/*
pub fn make_response<O>(o: Output<O>, mut r: ResponseBuilder) -> ZResult<Response<Body>> where O: Ser {
    let r = match o {
        Output::Empty =>
            Ok(r.body(Body::empty())?),
        Output::Raw(js_value) => {
            r.header(CONTENT_TYPE, APPLICATION_JSON.as_ref());
            let data =  serde_json::to_vec(&js_value)?;
            //let dlen = data.len();
            //req.header(hyper::header::CONTENT_LENGTH, dlen as u64);
            let body: Body = data.into();
            Ok(r.body(body)?)
        }
        Output::Typed(o) => {
            r.header(CONTENT_TYPE, APPLICATION_JSON.as_ref());
            let data =  serde_json::to_vec(&o)?;
            //let dlen = data.len();
            //req.header(hyper::header::CONTENT_LENGTH, dlen as u64);
            let body: Body = data.into();
            Ok(r.body(body)?)
        }
        Output::Stream(s, mime) => {
            r.header(CONTENT_TYPE, mime.as_ref());
            Ok(r.body(Body::wrap_stream(s.map_err(|e| Box::new(e))))?)
        }
        Output::Error(e) =>
            Err(e)

    };
    r.into()
}


pub fn make_request<O>(o: Output<O>, mut r: RequestBuilder) -> ZResult<Request<Body>> where O: Ser {
    let r = match o {
        Output::Empty =>
            Ok(r.body(Body::empty())?),
        Output::Raw(js_value) => {
            r.header(CONTENT_TYPE, APPLICATION_JSON.as_ref());
            let data =  serde_json::to_vec(&js_value)?;
            //let dlen = data.len();
            //req.header(hyper::header::CONTENT_LENGTH, dlen as u64);
            let body: Body = data.into();
            Ok(r.body(body)?)
        }
        Output::Typed(o) => {
            r.header(CONTENT_TYPE, APPLICATION_JSON.as_ref());
            let data =  serde_json::to_vec(&o)?;
            //let dlen = data.len();
            //req.header(hyper::header::CONTENT_LENGTH, dlen as u64);
            let body: Body = data.into();
            Ok(r.body(body)?)
        }
        Output::Stream(s, mime) => {
            r.header(CONTENT_TYPE, mime.as_ref());
            Ok(r.body(Body::wrap_stream(s.map_err(|e| Box::new(e))))?)
        }
        Output::Error(e) =>
            Err(e)

    };
    r.into()
}

pub fn parse_response<I>(r: Response<Body>, parse_as: InputType) -> Input<I> where I: Des + 'static {
    use hyper::body::Payload;
    fn check_content_type(r: Response<Body>) -> ZResult<Response<Body>> {
        let m = r.headers()
            .get(CONTENT_TYPE)
            .map(|s| s.to_str().map(|x| Mime::from_str(x)));
        match m {
            Some(Ok(Ok(ref ct))) if ct.type_() == APPLICATION && ct.subtype() == JSON =>
                Ok(r),
            Some(Ok(Ok(ref ct))) =>
                Err(rest_error!(other "invalid content type `{}`", ct)),
            Some(Ok(Err(ect))) =>
                Err(ect.into()),
            Some(Err(ect)) =>
                Err(ect.into()),
            None =>
                Err(rest_error!(other "no content type found (application/json required)"))
        }
    }

    fn as_json<I>(r: Response<Body>, typed: bool) -> Input<I> where I: Des + 'static {
        let y =
            future::result(check_content_type(r)).and_then(|res|
                res.into_body().concat2().map_err(|err| err.into())
            );
        if typed {
            Input::Typed(Box::new(y.and_then(|body| serde_json::from_slice(&body).map_err(|err| err.into()))))
        } else {
            Input::Raw(Box::new(y.and_then(|body| serde_json::from_slice(&body).map_err(|err| err.into()))))
        }
    }

    match parse_as {
        InputType::Empty =>
            if r.body().is_end_stream() {
                Input::Empty
            } else {
                Input::Error(rest_error!(other "Non-empty body where empty is expected"))
            }
        InputType::Raw => as_json(r, false),
        InputType::Typed => as_json(r, true),
        InputType::Stream => Input::Stream(Box::new(r.into_body().map_err(|e| e.into()))),
        InputType::Discard => Input::Empty
    }
}

pub fn parse_request<I>(r: Request<Body>, parse_as: InputType) -> Input<I> where I: Des + 'static {
    use hyper::body::Payload;
    fn check_content_type(r: Request<Body>) -> ZResult<Request<Body>> {
        let m = r.headers()
            .get(CONTENT_TYPE)
            .map(|s| s.to_str().map(|x| Mime::from_str(x)));
        match m {
            Some(Ok(Ok(ref ct))) if ct.type_() == APPLICATION && ct.subtype() == JSON =>
                Ok(r),
            Some(Ok(Ok(ref ct))) =>
                Err(rest_error!(other "invalid content type `{}`", ct)),
            Some(Ok(Err(ect))) =>
                Err(ect.into()),
            Some(Err(ect)) =>
                Err(ect.into()),
            None =>
                Err(rest_error!(other "no content type found (application/json required)"))
        }
    }

    fn as_json<I>(r: Request<Body>, typed: bool) -> Input<I> where I: Des + 'static {
        let y =
            future::result(check_content_type(r)).and_then(|res|
                res.into_body().concat2().map_err(|err| err.into())
            );
        if typed {
            Input::Typed(Box::new(y.and_then(|body| serde_json::from_slice(&body).map_err(|err| err.into()))))
        } else {
            Input::Raw(Box::new(y.and_then(|body| serde_json::from_slice(&body).map_err(|err| err.into()))))
        }
    }

    match parse_as {
        InputType::Empty =>
            if r.body().is_end_stream() {
                Input::Empty
            } else {
                Input::Error(rest_error!(other "Non-empty body where empty is expected"))
            }
        InputType::Raw => as_json(r, false),
        InputType::Typed => as_json(r, true),
        InputType::Stream => Input::Stream(Box::new(r.into_body().map_err(|e| e.into()))),
        InputType::Discard => Input::Empty
    }
}
*/