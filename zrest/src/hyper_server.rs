use futures::{Future, Stream};
use tokio_core::reactor::Core;

use hyper;
use hyper::{Request, Response, Body, Chunk, StatusCode, Uri, Method};
use hyper::header::{/*Authorization, Basic,*/ ContentType};

use serde;
use serde_json;

use ::*;
//--------------------------------------------------------------------------------------------------

const PHRASE: &'static str = "Hello, World!";

struct Router;

use hyper::server::Service;
use hyper::header::{ContentLength};

impl Service for Router {
    // boilerplate hooking up hyper's server types
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    // The future representing the eventual Response your call will
    // resolve to. This can change to whatever Future you need.
    type Future = Box<Future<Item=Self::Response, Error=Self::Error>>;

    fn call(&self, _req: Request) -> Self::Future {
        // We're currently ignoring the Request
        // And returning an 'ok' Future, which means it's ready
        // immediately, and build a Response with the 'PHRASE' body.
        Box::new(futures::future::ok(
            Response::new()
                .with_header(ContentLength(PHRASE.len() as u64))
                .with_body(PHRASE)
        ))
    }
}

