//! User-side server
extern crate hyper;
extern crate serde_json;

use std::sync::Mutex;
use std::fmt;
use std::collections::hash_map::HashMap;
use self::hyper::server::{Handler, Server, Request, Listening};
use self::hyper::server::Response as SResponse;
use self::hyper::status::StatusCode;
use self::hyper::method::Method;

use w::*;
use nv::{NodeValue, TsNodeValue};
use w_server::UWServerRunner;
use Result;



struct UWServerHandler {
    runner: Mutex<UWServerRunner>,
    nv_types: HashMap<String, String>
}

enum ApiOp {
    N,
    UGET(String),
    USET(String, String),
    ERR(String)
}

impl fmt::Display for ApiOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ApiOp::N =>
                write!(f, "N"),
            &ApiOp::UGET(ref id) =>
                write!(f, "UGET(id={})", id),
            &ApiOp::USET(ref id, ref value) =>
                write!(f, "USET(id={}, v={})", id, value),
            &ApiOp::ERR(ref s) =>
                write!(f, "ERR({})", s)
        }
    }
}

impl UWServerHandler {
    fn make_node_value(&self, id: &str, arg: String) -> Result<NodeValue> {
        let r0: Result<NodeValue> = arg.parse();

        r0.or_else(
            |e| self.nv_types
                .get(id)
                .map(move |t| NodeValue::from_strings(t, arg))
                .unwrap_or_else(move || Err(e))
        )
    }
}

impl Handler for UWServerHandler {
    fn handle(&self, req: Request, mut res: SResponse) {
        //TODO: check for content-type=application/json

        //debug!("Request md: `{}` `{}` `{}` `{}`", req.remote_addr, req.method, req.uri, req.headers);

        use self::hyper::uri::RequestUri;

        let op = match (&req.method, &req.uri) {
            (&Method::Post, &RequestUri::AbsolutePath(ref p)) if p == "/1/n" => ApiOp::N,
            (&Method::Get, &RequestUri::AbsolutePath(ref p)) if p.starts_with("/1/u/get/") => {
                ApiOp::UGET((&p[9..]).to_string())
            },
            (&Method::Get, &RequestUri::AbsolutePath(ref p)) if p.starts_with("/1/u/set/") => {
                let mut v = (&p[9..]).splitn(2, "/");
                let fid = v.next();
                let fvalue = v.next();
                match (fid, fvalue) {
                    (Some(id), Some(value)) => ApiOp::USET(id.to_string(), value.to_string()),
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
            ApiOp::UGET(id) => {
                let umsgr = self.runner.lock().map(
                        move |mut uws| uws.push(UserUpdate::new_get(id))
                    ).map_err(
                        |e| format!("Mutex poison error {}", e)
                    );

                debug!("[N] Reply: {:?}", umsgr);

                umsgr.and_then(
                    |umsg| serde_json::to_vec(&umsg)
                        .map_err(|e| format!("Error converting response: {}", e))
                )
            },
            ApiOp::USET(id, value) => {
                use std::time::{UNIX_EPOCH, SystemTime};
                let now_r =
                    SystemTime::now().duration_since(UNIX_EPOCH).map_err(|e| format!("Sys time err: {}", e));

                let nv_r = self.make_node_value(&id, value);

                let tnv_r =
                    now_r.and_then(move |now|
                        nv_r.map(move |nv|
                            TsNodeValue::new(nv, now.as_secs() as u32)
                        ));

                let uur = tnv_r.map(|tnv|UserUpdate::new_set(id, tnv));

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

pub fn run_uw_server_hyper<'t>(hostport: &'t str, sr: UWServerRunner, nv_types: HashMap<String, String>) -> Result<Listening> {
    Server::http(hostport).and_then(|srv| srv.handle(UWServerHandler {
        runner: Mutex::new(sr),
        nv_types: nv_types
    })).map_err(|e| format!("error listening: {}", e))
}
