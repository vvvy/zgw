extern crate futures;
extern crate hyper;
extern crate tokio_core;
extern crate hyper_tls;
extern crate serde;
#[macro_use]
extern crate serde_derive;
extern crate serde_json;
extern crate native_tls;

mod hyper_w;

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

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct R {
        operation: String,
        expression: String,
        result: String
    }
    #[test]
    fn it_works() {
        use hyper_w::*;
        //let mut c = ClientFactory::new();
        //let cn = c.tls_client(1).unwrap();
        let mut cl = ZHttpsClient::new(1).unwrap();
        let res = cl.get::<R>("https://newton.now.sh/factor/x%5E2-1".parse().unwrap()).unwrap();
        assert_eq!(res , R {
            operation: "factor".to_string(),
            expression: "x^2-1".to_string(),
            result: "(x - 1) (x + 1)".to_string()
        });
        println!("{:?}", res);
    }
}
