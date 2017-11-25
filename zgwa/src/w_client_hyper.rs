extern crate hyper;

use self::hyper::Client;
use self::hyper::status::StatusCode;
//use self::hyper::method::Method;
use self::hyper::client::response::Response;
use self::hyper::client::RequestBuilder;
use self::hyper::header::{Authorization, Basic};

use std::io::Read;
use w_client::*;
use w::*;
use zgwlib::Result;

extern crate serde_json;

pub struct HyperWClient {
    url: String
}

impl HyperWClient {
    pub fn new(url: String) -> HyperWClient {
        HyperWClient { url: format!("{}/1/n", url.trim_right_matches("/")) }
    }
}

fn intercept_errors(mut r: Response) -> Result<Response> {
    if r.status != StatusCode::Ok {
        let mut body = vec![];
        let bodytext = match r.read_to_end(&mut body) {
            Ok(_) => format!("{}", String::from_utf8_lossy(&body)),
            Err(e) => format!("Couldn't read body text: {}", e)
        };
        Err(format!("[S]HyperWClient: invalid response: {} `{}`", r.status, bodytext))
    } else {
        Ok(r)
    }
}

impl WClient for HyperWClient {
    fn do_request(&self, m: WNetMsg) -> Result<WUserMsg> {
        serde_json::to_string(&m).map_err(
            |e| format!("to-json error: {}", e)
        ).and_then( |body|
            Client::new().post(
                &self.url
            ).body(
                &body
            ).send().map_err(
                |e| format!("request error: {}", e)
            )
        ).and_then(
            intercept_errors
        ).and_then(
            |r| serde_json::from_reader(r).map_err(
                |e| format!("from-json error: {}", e)
            )
        )
    }
}

//------------------------------------------------------------
// Secure (TLS) connector
extern crate hyper_native_tls;

use self::hyper::net::HttpsConnector;
use self::hyper_native_tls::NativeTlsClient;

pub struct SecureHyperWClient {
    url: String,
    auth: Option<(String, String)>
}

impl SecureHyperWClient {
    pub fn new(url: String, auth: Option<(String, String)>) -> SecureHyperWClient {
        SecureHyperWClient { url: url, auth: auth }
    }
}

trait AddOptAuth {
    fn add_opt_auth(self, auth: &Option<(String, String)>) -> Self;
}

impl<'t> AddOptAuth for RequestBuilder<'t> {
    fn add_opt_auth(self, auth: &Option<(String, String)>) -> Self {
        match auth {
            &None => self,
            &Some((ref user, ref password)) => self.header(
                Authorization(
                    Basic {
                        username: user.clone(),
                        password: Some(password.clone())
                    }
                )
            )
        }
    }
}


impl WClient for SecureHyperWClient {
    fn do_request(&self, m: WNetMsg) -> Result<WUserMsg> {
        NativeTlsClient::new().map_err(
            |e| format!("SecureHyperWClient: TLS initialization error: {}", e)
        ).and_then(|ssl|
            serde_json::to_string(&m).map_err(
                |e| format!("to-json error: {}", e)
            ).and_then(|body|
                Client::with_connector(
                    HttpsConnector::new(ssl)
                ).post(
                    &self.url
                ).body(
                    &body
                ).add_opt_auth(&self.auth).send().map_err(
                    |e| format!("request error: {}", e)
                )
            )
        ).and_then(
            intercept_errors
        ).and_then(
            |r| serde_json::from_reader(r).map_err(
                |e| format!("from-json error: {}", e)
            )
        )
    }
}
