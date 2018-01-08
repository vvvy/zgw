extern crate hyper;

use self::hyper::Client;
use self::hyper::status::StatusCode;
use self::hyper::client::response::Response;
use self::hyper::client::RequestBuilder;
use self::hyper::header::{Authorization, Basic};

use std::io::Read;
use std::fmt::Write;
use z_client::*;
use zgwlib::*;

pub struct HyperZClient {
    urlbase: String
}

impl HyperZClient {
    pub fn new(urlbase: String) -> HyperZClient {
        HyperZClient { urlbase: urlbase }
    }
}

fn intercept_errors(mut r: Response) -> Result<Response> {
    if r.status != StatusCode::Ok {
        let mut body = vec![];
        let bodytext = match r.read_to_end(&mut body) {
            Ok(_) => format!("{}", String::from_utf8_lossy(&body)),
            Err(e) => format!("Couldn't read body text: {}", e)
        };
        Err(Error::gen(&format!("HyperConnector: invalid response: {} `{}`", r.status, bodytext)))
    } else {
        Ok(r)
    }
}
//yet nothing to intercept
fn intercept_app_errors(_r: Response) -> Result<()> { Ok(()) }

fn zw_data_string(urlbase: &String, ts: u32) -> String {
    format!("{}/Data/{}", urlbase, ts)
}
fn zw_cmd_string(urlbase: &String, target: OperationTarget, cmd: DeviceOperation) -> String {
    //ZWaveAPI/Run/devices[2].instances[0].commandClasses[0x20].Set(255)
    let mut s = String::new();
    let _ = write!(&mut s, "{}/Run/devices[{}].instances[{}].commandClasses[{}].",
           urlbase, target.device_id, target.instance_id, target.command_class);
    let _ = match cmd {
        DeviceOperation::Get => write!(&mut s, "Get()"),
        DeviceOperation::GetP(m) => write!(&mut s, "Get({})", m),
        DeviceOperation::Set(v) => write!(&mut s, "Set({})", v),
        DeviceOperation::SetP(m, v) => write!(&mut s, "Set({},{})", m, v),
        DeviceOperation::SetFloat(v) => write!(&mut s, "Set({})", v),
        DeviceOperation::SetFloatP(m, v) => write!(&mut s, "Set({},{})", m, v)
    };
    s
}

impl ZClient for HyperZClient {
    fn collect_updates(&self, ts: u32, sk: &mut ZNotificationTarget) -> Result<()> {
        Client::new().get(
            &zw_data_string(&self.urlbase, ts)
        ).send().map_err(
            |e| Error::other("HyperConnector: request error", e)
        ).and_then(
            intercept_errors
        ).and_then(
            |mut r| sk.push(&mut r)
        )
    }
    fn exec(&self, target: OperationTarget, op: DeviceOperation) -> Result<()> {
        Client::new().get(
            &zw_cmd_string(&self.urlbase, target, op)
        ).send().map_err(
            |e| Error::other("HyperConnector: request error", e)
        ).and_then(
            intercept_errors
        ).and_then(
            intercept_app_errors
        )
    }
}

//------------------------------------------------------------
// Secure (TLS) connector
extern crate hyper_native_tls;

use self::hyper::net::HttpsConnector;
use self::hyper_native_tls::NativeTlsClient;

pub struct SecureHyperZClient {
    urlbase: String,
    auth: Option<(String, String)>
}

impl SecureHyperZClient {
    pub fn new(urlbase: String, auth: Option<(String, String)>) -> SecureHyperZClient {
        SecureHyperZClient { urlbase: urlbase, auth: auth }
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


impl ZClient for SecureHyperZClient {
    fn collect_updates(&self, ts: u32, sk: &mut ZNotificationTarget) -> Result<()> {
        NativeTlsClient::new().map_err(
            |e| Error::other("HyperConnector: TLS initialization error", e)
        ).and_then(
            |ssl|
                Client::with_connector(HttpsConnector::new(ssl)).get(
                    &zw_data_string(&self.urlbase, ts)
                ).add_opt_auth(
                    &self.auth
                ).send(
                ).map_err(
                    |e| Error::other("HyperConnector: request error", e)
                ).and_then(
                    intercept_errors
                ).and_then(
                    |mut r| sk.push(&mut r)
                )
        )
    }
    fn exec(&self, target: OperationTarget, op: DeviceOperation) -> Result<()> {
        NativeTlsClient::new().map_err(
            |e| Error::other("HyperConnector: TLS initialization error", e)
        ).and_then(
            |ssl|
                Client::with_connector(HttpsConnector::new(ssl)).get(
                    &zw_cmd_string(&self.urlbase, target, op)
                ).add_opt_auth(
                    &self.auth
                ).send(
                ).map_err(
                    |e| Error::other("HyperConnector: request error", e)
                ).and_then(
                    intercept_errors
                ).and_then(
                    intercept_app_errors
                )
        )
    }
}