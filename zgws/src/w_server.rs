use std::sync::mpsc::Sender;
//use nv::*;
//use nq::*;
use w::*;

pub struct UWServerRunner {
    ep: Endpoint<UserUpdate, Sender<UWEvt>>,
    count_max: usize
}

impl UWServerRunner {
    pub fn new(target: Sender<UWEvt>, count_max: usize) -> UWServerRunner {
        UWServerRunner { ep: Endpoint::new(target), count_max: count_max }
    }

    pub fn handle_request(&mut self, m: WNetMsg) -> WUserMsg {
        let (ud, more) = self.ep.rr(m.nd, self.count_max);
        WUserMsg { ud: ud, more: more }
    }

    pub fn push(&mut self, uu: UserUpdate) {
        self.ep.push(uu)
    }
}
