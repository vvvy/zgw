use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};
use std::sync::mpsc::{Sender, Receiver, RecvTimeoutError};

use nv::*;
use w::*;
use zgwlib::Result;

//use zgwlib::WCmd::NetUpdate;

pub struct WConfig {
    pub max_send_count: usize,
    pub period_long: Duration,
    pub period_short: Duration,
    pub delay_error: Duration
}

struct StreamShaper {
    q: QS,
    t: Instant,
    cfg: WConfig
}

#[derive(Clone, Copy, PartialEq)]
enum QS {
    Null,
    Empty,
    Short,
    Recovery
}

impl StreamShaper {
    fn new(cfg: WConfig) -> StreamShaper {
        StreamShaper {
            q: QS::Null,
            t: Instant::now(),
            cfg: cfg
        }
    }

    fn get_st(&self, q_size: usize) -> QS {
        match q_size {
            0 => QS::Empty,
            n if n < self.cfg.max_send_count => QS::Short,
            _ => QS::Null
        }
    }

    fn get_sleep_time(&mut self, q_size: usize) -> Option<Duration> {
        use self::QS::*;
        let now = Instant::now();
        let q = self.get_st(q_size);
        if self.q == Recovery && now < self.t {
            Some(self.t - now)
        } else {
            let rv = match (q, self.q) {
                (_, Recovery) => None,
                (Empty, Empty) => if now < self.t { Some(self.t - now) } else { None },
                (Empty, _) => {
                    self.t = now + self.cfg.period_long;
                    Some(self.cfg.period_long)
                },
                (Short, Short) => if now < self.t { Some(self.t - now) } else { None },
                (Short, _) => {
                    self.t = now + self.cfg.period_short;
                    Some(self.cfg.period_short)
                },
                (Null, _) => { None },
                (_, _) => { error!("Should never get here"); None }
            };
            self.q = if rv.is_none() { Null } else { q };
            rv
        }
    }

    fn error(&mut self) {
        self.q = QS::Recovery;
        self.t = Instant::now() + self.cfg.delay_error;
    }

    fn more_remote_input(&mut self) {
        self.q = QS::Recovery;
        self.t = Instant::now() + self.cfg.period_short;
    }
}


pub trait WClient: Send {
    fn do_request(&self, m: WNetMsg) -> Result<WUserMsg>;
}

pub struct WClientRunner {
    ep: Endpoint<NetUpdate, Sender<WCmd>>,
    ss: StreamShaper,
    c: Box<WClient>
}

impl WClientRunner {
    fn new(client: Box<WClient>, sender: Sender<WCmd>, cfg: WConfig) -> WClientRunner {
        WClientRunner {
            ep: Endpoint::new(sender),
            ss: StreamShaper::new(cfg),
            c: client
        }
    }


    fn push(&mut self, e: NetUpdate) {
        self.ep.push(e)
    }

    /// pushes out queue contents.
    /// Returns duration to sleep until next invocation, or None on error (then caller should exit)
    fn run(&mut self) -> Option<Duration> {
        loop {
            match self.ss.get_sleep_time(self.ep.q_size()) {
                Some(d) => return Some(d),
                None => {
                    let (nd, _) = self.ep.produce(self.ss.cfg.max_send_count);
                    match self.c.do_request(WNetMsg { nd: nd }) {
                        Ok(WUserMsg { ud, more }) => {
                            if !self.ep.consume(ud) { return None; }
                            if more { self.ss.more_remote_input(); }
                        },
                        Err(e) => {
                            error!("request error: {}", e);
                            self.ss.error();
                        }
                    }
                }
            }
        }
    }
}

pub fn run_w_client(
    cfg: WConfig,
    w_recv: Receiver<WCmd>,
    z_send: Sender<WCmd>,
    client: Box<WClient>) -> JoinHandle<()> {

    thread::spawn(move || {
        let mut wcr = WClientRunner::new(client, z_send, cfg);

        while let Some(d) = wcr.run() {
            match w_recv.recv_timeout(d) {
                Ok(WCmd::NetUpdate(l, tnv)) => {
                    wcr.push(NetUpdate(l, tnv));
                },
                Ok(cmd) => {
                    error!("run_w_client: invalid WCmd {})", cmd);
                },
                Err(RecvTimeoutError::Timeout) => {
                    ()
                },
                Err(RecvTimeoutError::Disconnected) => {
                    error!("run_w_client: peer disconnected, exiting");
                    break;
                }
            }
        }
    })
}
