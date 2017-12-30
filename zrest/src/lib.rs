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
pub mod server;
mod uri_scanner;
mod router;

use std::error::Error as StdError;
use std::fmt;

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
            ref e => f.write_str(e.description())
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
            _ => "Unknown error"
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
    use server::*;
}