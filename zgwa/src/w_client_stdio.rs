//use std::io::Read;
use w_client::*;
use w::*;
use zgwlib::Result;

//extern crate serde_json;

pub struct StdioWClient {

}

impl WClient for StdioWClient {
    fn do_request(&self, m: WNetMsg) -> Result<WUserMsg> {
        info!("LOG_MSG: {:?}", m);
        Ok(WUserMsg::acked(&m))
    }
}