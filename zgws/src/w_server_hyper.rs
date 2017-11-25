//! User-side server
extern crate hyper;
extern crate serde_json;

use std::sync::Mutex;
use std::fmt;
use self::hyper::server::{Handler, Server, Request, Listening};
use self::hyper::server::Response as SResponse;
use self::hyper::status::StatusCode;
use self::hyper::method::Method;

use w::*;
use w_server::UWServerRunner;
use Result;



struct UWServerHandler {
    runner: Mutex<UWServerRunner>
}

enum ApiOp {
    N,
    U(String, String),
    ERR(String)
}

impl fmt::Display for ApiOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ApiOp::N =>
                write!(f, "N"),
            &ApiOp::U(ref id, ref value) =>
                write!(f, "U(id={}, v={})", id, value),
            &ApiOp::ERR(ref s) =>
                write!(f, "ERR({})", s)
        }
    }
}

impl Handler for UWServerHandler {
    fn handle(&self, req: Request, mut res: SResponse) {
        //TODO: check for content-type=application/json

        //debug!("Request md: `{}` `{}` `{}` `{}`", req.remote_addr, req.method, req.uri, req.headers);

        use self::hyper::uri::RequestUri;

        let op = match (&req.method, &req.uri) {
            (&Method::Post, &RequestUri::AbsolutePath(ref p)) if p == "/1/n" => ApiOp::N,
            (&Method::Get, &RequestUri::AbsolutePath(ref p)) if p.starts_with("/1/u/u/") => {
                let mut v = (&p[6..]).splitn(2, "/");
                let fid = v.next();
                let fvalue = v.next();
                match (fid, fvalue) {
                    (Some(id), Some(value)) => ApiOp::U (id.to_string(), value.to_string()),
                    _ => ApiOp::ERR(p.clone())
                }
            },
            (ref m, ref u) => ApiOp::ERR(format!("Invalid request {} {}", m, u))
        };

        let umsgbr = match op {
            ApiOp::N => {
                let nmsgr: Result<WNetMsg> =
                    serde_json::from_reader(req).map_err(|e| format!("Error converting request: {}", e));

                debug!("[N] Request: {:?}", nmsgr);

                let umsgr = nmsgr.and_then(|nmsg|
                    self.runner.lock().map(
                        |mut uws| uws.handle_request(nmsg)
                    ).map_err(
                        |e| format!("Mutex poison error {}", e)
                    )
                );

                debug!("[N] Reply: {:?}", umsgr);

                umsgr.and_then(
                    |umsg| serde_json::to_vec(&umsg)
                        .map_err(|e| format!("Error converting response: {}", e))
                )
            },
            ApiOp::U(id, value) => {
                use std::time::{UNIX_EPOCH, SystemTime};
                let now =
                    SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| format!("Sys time err: {}", e));
                let uur =
                    now.and_then(|d| UserUpdate::from_strings_and_ts(id, &value, d.as_secs() as u32));

                let umsgr = uur.and_then(
                    |uu| self.runner.lock().map(
                        move |mut uws| uws.push(uu)
                    ).map_err(
                        |e| format!("Mutex poison error {}", e)
                    ));

                debug!("[N] Reply: {:?}", umsgr);

                umsgr.and_then(
                    |umsg| serde_json::to_vec(&umsg)
                        .map_err(|e| format!("Error converting response: {}", e))
                )
            },
            ref x => Err(format!("invalid request-uri `{}`", x))

        };

        let send_result = match umsgbr {
            Ok(umsgb) =>
                res.send(&umsgb),
            Err(e) => {
                error!("Error processing request: {}", e);
                *res.status_mut() = StatusCode::BadRequest;
                res.send(&format!("error: {}", e).into_bytes())
            }
        };

        match send_result {
            Ok(_) => (),
            Err(e) => error!("Error sending response: {}", e)
        }
    }
}

pub fn run_uw_server_hyper<'t>(hostport: &'t str, sr: UWServerRunner) -> Result<Listening> {
    Server::http(hostport).and_then(|srv| srv.handle(UWServerHandler {
        runner: Mutex::new(sr)
    })).map_err(|e| format!("error listening: {}", e))
}
