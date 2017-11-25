use std::thread::{self, JoinHandle};
use tree::{TreeNode, CoTrigger, new_update_collector};
use std::time::Duration;
use std::io::Read;
use std::sync::mpsc::{Receiver, Sender, RecvTimeoutError};
pub use nv::{DeviceOperation, OperationTarget};
use nv::*;
use nvimpl::*;
use json;
use zgwlib::Result;

pub trait ZNotificationTarget {
    fn push<'t>(&'t mut self, &'t mut Read) -> Result<()>;
}

pub trait ZClient: Send {
    fn collect_updates<'t>(&'t self, u32, &'t mut ZNotificationTarget) -> Result<()>;
    fn exec(&self, target: OperationTarget, op: DeviceOperation) -> Result<()>;
}

struct ZNotificationTargetImpl<'t> {
    root: &'t mut TreeNode,
    jp: json::JsonParser,
    ct: &'t mut CoTrigger
}

impl<'t> ZNotificationTargetImpl<'t> {
    fn new(tn: &'t mut TreeNode, ct: &'t mut CoTrigger) -> ZNotificationTargetImpl<'t> {
        ZNotificationTargetImpl { root: tn, jp: json::JsonParser::new(), ct: ct }
    }
}

impl<'q> ZNotificationTarget for ZNotificationTargetImpl<'q> {
    fn push<'t>(&'t mut self, r: &'t mut Read) -> Result<()> {
        self.jp.parse_update(r).map(|data| self.root.merge_multi(data, self.ct))
    }
}

//-------------------------------------------------------------------------------------------------

pub struct ZConfig {
    pub delay: Duration,
    pub period_min: Duration,
    pub period_max: Duration,
}

pub fn run_z_client(cfg: ZConfig,
                    z_recv: Receiver<WCmd>,
                    w_send: Sender<WCmd>,
                    client: Box<ZClient>,
                    device_config: DeviceConfig) -> JoinHandle<()> {
    thread::spawn(move ||{
        let ((mut u, md), mut root) = configure(device_config, client.as_ref(), w_send);
        let mut delay = cfg.delay;
        loop {
            match z_recv.recv_timeout(delay) {
                Ok(WCmd::UserUpdate(n, tnv)) =>
                    u.process_event(&md, &n, &Event::UserUpdate(tnv)),
                Ok(WCmd::UserGet(n)) =>
                    u.process_event(&md, &n, &Event::UserGet),
                Ok(WCmd::NetUpdate(n, tnv)) =>
                    error!("Invalid NetUpdate({},{})", n, tnv),
                Err(RecvTimeoutError::Timeout) => {
                    //apply tree updates
                    let mut uc = new_update_collector();
                    match client.collect_updates(
                        root.get_update_time().unwrap_or(0),
                        &mut (ZNotificationTargetImpl::new(&mut root, &mut uc))
                    ) {
                        Ok(_) =>
                            if uc.updates.is_empty() {
                                debug!("get_updates: empty")
                            } else {
                                info!("get_updates: {:?}", uc.updates);
                                for (n, v) in uc.updates {
                                    match v.to_ts_node_value() {
                                        Some(tnv) =>
                                            u.process_event(&md, &n, &Event::NetUpdate(tnv)),
                                        None =>
                                            error!("Invalid update for '{}'", n)
                                    }
                                }
                            },
                        Err(e) =>
                            error!("get_updates: ERROR {}", e)
                    }
                    //run timers
                    u.run_timers(&md);
                },
                Err(RecvTimeoutError::Disconnected) => {
                    error!("run_z_client: peer disconnected, exiting");
                    break
                },
            }
            delay = u.get_delay(cfg.period_min, cfg.period_max);
        }
    })
}

