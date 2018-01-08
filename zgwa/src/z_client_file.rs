use std::fs::File;

use z_client::*;
use zgwlib::Result;

pub struct FileZClient {
    path: String
}

impl FileZClient {
    pub fn new(path: String) -> FileZClient {
        FileZClient { path: path }
    }
}

impl ZClient for FileZClient {
    fn collect_updates(&self, ts: u32, sk: &mut ZNotificationTarget) -> Result<()> {
        File::open(format!("{}/{}.json", self.path, ts)).map_err(
            |e| ::Error::io("FileConnector", e)
        ).and_then(
            |mut f| sk.push(&mut f)
        )
    }
    fn exec(&self, target: OperationTarget, op: DeviceOperation) -> Result<()> {
        info!("DeviceOperation: {:?} {:?}", target, op);
        Ok(())
    }
}

