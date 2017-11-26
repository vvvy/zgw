
use std::sync::mpsc::Sender;
use std::fmt;

use nq::*;
use nv::*;
use Result;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum UserUpdate {
    Set(String, TsNodeValue),
    Get(String)
}

impl UserUpdate {
    pub fn new_set(id: String, value: TsNodeValue) -> UserUpdate { UserUpdate::Set(id, value) }
    /*pub fn make_set(id: String, value: &str, ts: u32) -> Result<UserUpdate> {
        TsNodeValue::from_string_and_ts(value, ts).map(|tnv| UserUpdate::Set(id, tnv))
        //value.parse().map(|v| UserUpdate(id, v))
    }*/

    pub fn new_get(id: String) -> UserUpdate { UserUpdate::Get(id) }
}

impl AsRef<String> for UserUpdate {
    fn as_ref(&self) -> &String {
        match self {
            &UserUpdate::Get(ref id) => id,
            &UserUpdate::Set(ref id, _) => id,
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NetUpdate(pub String, pub TsNodeValue);

impl AsRef<String> for NetUpdate {
    fn as_ref(&self) -> &String { &self.0 }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FdxMsg<T> {
    d: Vec<T>,
    n: usize,
    rn: usize
}

pub trait Target<T> {
    fn consume(&self, d: Vec<T>) -> Result<()>;
}

impl Target<NetUpdate> for Sender<WCmd> {
    fn consume(&self, d: Vec<NetUpdate>) -> Result<()> {
        debug!("Sending {} WCmds", d.len());
        for NetUpdate(l, tnv) in d {
            match self.send(WCmd::NetUpdate(l, tnv)) {
                Ok(_) => (),
                Err(_) => return Err("feed failed (Disconnect)".to_string())
            }
        }
        Ok(())
    }
}

impl Target<UserUpdate> for Sender<WCmd> {
    fn consume(&self, d: Vec<UserUpdate>) -> Result<()> {
        /*d.into_iter().map(|uu| match uu {
            UserUpdate::Set(id, tnv) => WCmd::UserUpdate(id, tnv),
            UserUpdate::Get(id) => WCmd::UserGet(id)
        }).for_each(|wcmd| match self.send(wcmd) {
            Ok(_) => (),
            Err(_) => return Err("feed failed (Disconnect)".to_string())
        });

        */
        for uu in d {
            let wcmd = match uu {
                UserUpdate::Set(id, tnv) => WCmd::UserUpdate(id, tnv),
                UserUpdate::Get(id) => WCmd::UserGet(id)
            };
            match self.send(wcmd) {
                Ok(_) => (),
                Err(_) => return Err("feed failed (Disconnect)".to_string())
            }
        }
        Ok(())
    }
}

/// UW Interface event
pub enum UWEvt {
    NetUpdate(String, TsNodeValue),
    Heartbeat
}

impl fmt::Display for UWEvt {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &UWEvt::NetUpdate(ref l, ref tnv) => write!(f, "NetUpdate({}, {})", l, tnv),
            &UWEvt::Heartbeat => write!(f, "Heartbeat")
        }
    }
}

impl Target<NetUpdate> for Sender<UWEvt> {
    fn consume(&self, d: Vec<NetUpdate>) -> Result<()> {
        debug!("Sending {} NetUpdates", d.len());

        let st = if d.is_empty() {
            self.send(UWEvt::Heartbeat)
        } else {
            d.into_iter().fold(Ok(()), |r, NetUpdate(l, tnv)|
                r.and_then(|_| self.send(UWEvt::NetUpdate(l, tnv)))
            )
        };

        st.map_err(|e| format!("feed failed (Disconnect): {}", e))
    }
}


/// Full duplex endpoint
pub struct Endpoint<L, T> {
    sq: SQ<String, L>,
    qrecv: QRecv,
    t: T
}

impl<L, T> Endpoint<L, T> where L: AsRef<String> + Clone {
    pub fn new(t: T) -> Endpoint<L, T> {
        Endpoint { sq: SQ::new(), qrecv: QRecv::new(), t: t }
    }

    pub fn push(&mut self, e: L) {
        self.sq.push(e)
    }

    pub fn q_size(&self) -> usize {
        self.sq.q_size()
    }

    /// consumes data in m; returns true if ok or false on error
    pub fn consume<R>(&mut self, m: FdxMsg<R>) -> bool where T: Target<R> {
        if !self.sq.ack(m.rn) {
            warn!("sync lost at rn={}", m.rn);
        }
        let ref t = self.t;
        match self.qrecv.feed_through(Feed(m.d, m.n), |v| t.consume(v)) {
            Err(e) => {
                error!("consume: {}", e);
                false
            }
            Ok(_) => { true }
        }
    }

    pub fn produce(&mut self, count_max: usize) -> (FdxMsg<L>, bool) {
        let (Feed(d, n), more) = self.sq.get_feed(count_max);
        (FdxMsg { d: d, n: n, rn: self.qrecv.acked() }, more)
    }

    /// Consumes messages from m, returning pending messages from the queue, wrapped in FdxMsg
    pub fn rr<R>(&mut self, m: FdxMsg<R>, count_max: usize) -> (FdxMsg<L>, bool) where T: Target<R> {
        self.consume(m);
        self.produce(count_max)
    }
}


/// Network-initiated W interface message (WN)
#[derive(Debug, Serialize, Deserialize)]
pub struct WNetMsg {
    pub nd: FdxMsg<NetUpdate>,
}

/// User-initiated W interface message (WU)
#[derive(Debug, Serialize, Deserialize)]
pub struct WUserMsg {
    pub ud: FdxMsg<UserUpdate>,
    pub more: bool
}

impl WUserMsg {
    pub fn acked(nm: &WNetMsg) -> WUserMsg {
        WUserMsg { ud: FdxMsg { d: Vec::new(), n: 0, rn: nm.nd.n }, more: false }
    }
}
