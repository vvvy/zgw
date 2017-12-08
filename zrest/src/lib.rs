extern crate futures;
extern crate hyper;
extern crate tokio_core;
extern crate hyper_tls;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate native_tls;
extern crate mime;

mod hyper_w;
mod hyper_server;

#[derive(Debug)]
pub enum ZRError {
    EHyper(hyper::Error),
    EHTLS(native_tls::Error),
    ESJ(serde_json::Error),
    EIO(std::io::Error),
    EZR(String)
}

pub type ZRResult<T> = Result<T, ZRError>;

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


#[cfg(test)]
mod tests {
    use hyper_w::*;

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
        userId: i32
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct PubResp {
        title: String,
        body: String,
        userId: i32,
        id: i32
    }

    #[test]
    fn test_post_s() {
        let mut cl = ZHttpsClient::new(1).unwrap();
        let res = cl.post::<PubReq, PubResp>(
            "https://jsonplaceholder.typicode.com/posts".parse().unwrap(),
            &PubReq { title: "ABC".to_string(), body: "DEF".to_string(), userId: 111 },
            &None
        ).unwrap();
        assert_eq!(res , PubResp {
            title: "ABC".to_string(), body: "DEF".to_string(), userId: 111,
            id: 101
        });
        println!("{:?}", res);
    }

    #[test]
    fn test_post() {
        let mut cl = ZHttpClient::new().unwrap();
        let res = cl.post::<PubReq, PubResp>(
            "http://jsonplaceholder.typicode.com/posts".parse().unwrap(),
            &PubReq { title: "ABC".to_string(), body: "DEF".to_string(), userId: 111 },
            &None
        ).unwrap();
        assert_eq!(res , PubResp {
            title: "ABC".to_string(), body: "DEF".to_string(), userId: 111,
            id: 101
        });
        println!("{:?}", res);
    }

}
