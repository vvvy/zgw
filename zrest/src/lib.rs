extern crate futures;
extern crate hyper;
extern crate tokio_core;
extern crate hyper_tls;
extern crate serde;
#[macro_use] extern crate serde_derive;
#[macro_use] extern crate serde_json;
extern crate native_tls;
extern crate mime;

pub mod client;
mod uri_scanner;
#[macro_use] pub mod server;
#[macro_use] mod router;
pub mod wrappers;

use std::error::Error as StdError;
use std::fmt;



pub use server::*;
pub use client::*;
pub use router::*;
pub use wrappers::*;
pub use serde_json::Value as JsValue;
pub use serde::ser::Serialize as Ser;
pub use serde::de::DeserializeOwned as Des;

#[derive(Debug)]
pub enum ZRError {
    EHyper(hyper::Error),
    EHTLS(native_tls::Error),
    ESJ(serde_json::Error),
    EIO(std::io::Error),
    ESc(uri_scanner::SErr),
    EZR(String)
}

impl fmt::Display for ZRError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ZRError::EHyper(ref e) => fmt::Display::fmt(e, f),
            ZRError::EHTLS(ref e) => fmt::Display::fmt(e, f),
            ZRError::ESJ(ref e) => fmt::Display::fmt(e, f),
            ZRError::EIO(ref e) => fmt::Display::fmt(e, f),
            ZRError::ESc(ref e) => fmt::Display::fmt(e, f),
            ZRError::EZR(ref s) => fmt::Display::fmt(s, f),
            //ref e => f.write_str(e.description())
        }
    }
}

impl From<hyper::Error> for ZRError {
    fn from(e: hyper::Error) -> Self { ZRError::EHyper(e) }
}

impl From<native_tls::Error> for ZRError {
    fn from(e: native_tls::Error) -> Self { ZRError::EHTLS(e) }
}

impl From<serde_json::Error> for ZRError {
    fn from(e: serde_json::Error) -> Self { ZRError::ESJ(e) }
}

impl From<std::io::Error> for ZRError {
    fn from(e: std::io::Error) -> Self { ZRError::EIO(e) }
}

impl From<uri_scanner::SErr> for ZRError {
    fn from(e: uri_scanner::SErr) -> Self { ZRError::ESc(e) }
}

/*
impl From<ZRError> for hyper::Error {
    fn from(e: ZRError) -> Self { hyper::Error:: }
}
*/

impl StdError for ZRError {
    fn description(&self) -> &str {
        match *self {
            ZRError::EHyper(ref e) => e.description(),
            ZRError::EHTLS(ref e) => e.description(),
            ZRError::ESJ(ref e) => e.description(),
            ZRError::EIO(ref e) => e.description(),
            ZRError::ESc(ref e) => e.description(),
            ZRError::EZR(ref s) => s,
            //_ => "Unknown error"
        }
    }

    fn cause(&self) -> Option<&StdError> {
        match *self {
            ZRError::EHyper(ref he) => Some(he),
            _ => None
        }
    }
}

pub type ZRResult<T> = Result<T, ZRError>;

#[cfg(test)]
mod client_tests {
    use client::*;

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct R {
        operation: String,
        expression: String,
        result: String
    }
    #[test]
    fn test_get_s() {
        let mut cl = ZHttpsClient::new(1).unwrap();
        let res = cl.get::<R>("https://newton.now.sh/factor/x%5E2-1".parse().unwrap()).unwrap();
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
        user_id: i32
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
        let mut cl = ZHttpsClient::new(1).unwrap();
        let res = cl.post::<PubReq, PubResp>(
            "https://jsonplaceholder.typicode.com/posts".parse().unwrap(),
            &PubReq { title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111 },
            &None
        ).unwrap();
        assert_eq!(res , PubResp {
            title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111,
            id: 101
        });
        println!("{:?}", res);
    }

    #[test]
    fn test_post() {
        let mut cl = ZHttpClient::new().unwrap();
        let res = cl.post::<PubReq, PubResp>(
            "http://jsonplaceholder.typicode.com/posts".parse().unwrap(),
            &PubReq { title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111 },
            &None
        ).unwrap();
        assert_eq!(res , PubResp {
            title: "ABC".to_string(), body: "DEF".to_string(), user_id: 111,
            id: 101
        });
        println!("{:?}", res);
    }

}




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

            let b = route_builder!(
                AsyncActionFn,
                "v0/static" => AsyncActionFn::constant::<JsValue, JsValue, _>(|| json!({"static":"true", "message": "v0/static"})),
                "v0/str/$/a" => AsyncActionFn::v_r::<String, JsValue, JsValue, _>(|s| Ok(json!({"op":"a", "type": "str", "vars": [{"str" : s}] }))),
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
            let b = route_builder!(
                AsyncActionFn,
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
        println!("### {:?}", res);
        let res = cl.post::<TIO, GOW<TIO>>("http://127.0.0.1:3002/v0/nm/44".parse().unwrap(), &TIO { s: "abc".to_owned(), i: 2 }, &None).unwrap();
        println!("### {:?}", res);
        snd.send(()).unwrap();
    }

}