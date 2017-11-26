use std::time::{Duration, Instant};
use std::collections::{HashMap, HashSet, BTreeSet};
use std::mem::swap;
use std::fmt;
use std::cmp::{min, max};
use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum NodeValue {
    Float(f64),
    Int(i64),
    Bool(bool),
    String(String),
    StringArray(Vec<String>),
    IntArray(Vec<i64>),
    Binary(Vec<u8>),
    Null
}

impl fmt::Display for NodeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fn wa<A : fmt::Display>(f: &mut fmt::Formatter, v: &Vec<A>) -> fmt::Result {
            (*v).iter().fold(
                write!(f, "["),
                |r, ref i| r.and_then(|_|write!(f, "{},", i))
            ).and_then(|_|write!(f,"]"))
        }
        match *self {
            NodeValue::Float(v) => write!(f, "Float({})", v),
            NodeValue::Int(v) => write!(f, "Int({})", v),
            NodeValue::Bool(v) => write!(f, "Bool({})", v),
            NodeValue::String(ref v) => write!(f, "String({})", v),
            NodeValue::StringArray(ref v) => { write!(f, "StringArray").and_then(|_| wa(f, v)) },
            NodeValue::IntArray(ref v) => { write!(f, "IntArray").and_then(|_| wa(f, v)) },
            NodeValue::Binary(ref v) => { write!(f, "Binary").and_then(|_| wa(f, v)) },
            NodeValue::Null => write!(f, "Null")
        }
    }
}

impl NodeValue {
    pub fn from_strings(k: &str, v: String) -> Result<NodeValue, String> {
        if k == "String" { Ok(NodeValue::String(v)) }
            else if k == "Float" { v.parse().map(|v| NodeValue::Float(v)).map_err(|e| format!("Float parse: {}", e)) }
                else if k == "Int" { v.parse().map(|v| NodeValue::Int(v)).map_err(|e| format!("Int parse: {}", e)) }
                    else if k == "Bool" {
                        if v == "true" { Ok(NodeValue::Bool(true)) }
                            else if v == "false" { Ok(NodeValue::Bool(false)) }
                                else { Err(format!("Bool parse: invalid constant `{}`", v))}
                    }
                        //TODO: StringArray, IntArray, Binary
                        else { Err(format!("Invalid NV: k={}, v={}", k, v)) }
    }

    pub fn from_string(k: &str) -> Result<NodeValue, String> {
        if k == "Null" { Ok(NodeValue::Null) }
            else { Err(format!("Invalid NV: k={}", k)) }
    }
}

impl FromStr for NodeValue {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let it = s.chars();

        enum S {
            K(String),
            KV(String, String),
            R(String, String),
            ERR(String)
        }

        fn plus(mut s: String, ch: char) -> String {
            s.push(ch);
            s
        }

        match it.fold(
            S::K("".to_string()),
            |s, v| match (s, v) {
                (S::K(k), '(') => S::KV(k, "".to_string()),
                (S::K(k), ch) => S::K(plus(k, ch)),
                (S::KV(k, v), ')') => S::R(k, v),
                (S::KV(k, v), ch) => S::KV(k, plus(v, ch)),
                (err @ S::ERR(_), _) => err,
                (_, ch) => S::ERR(format!("parse error at `{}`", ch))
            }
        ) {
            S::R(k, v) => NodeValue::from_strings(&k, v),
            S::K(k) => NodeValue::from_string(&k),
            _ => Err("Invalid NV format".to_string())
        }
    }
}

#[test]
fn test_parse_nv() {
    assert_eq!(Ok(NodeValue::Bool(true)), "Bool(true)".parse::<NodeValue>());
    assert_eq!(Ok(NodeValue::Int(101)), "Int(101)".parse::<NodeValue>());
    assert_eq!(Ok(NodeValue::Float(10.2)), "Float(10.2)".parse::<NodeValue>());
    assert_eq!(Ok(NodeValue::String("sx".to_string())), "String(sx)".parse::<NodeValue>());
    assert_eq!(Ok(NodeValue::Null), "Null".parse::<NodeValue>());
    assert!("Float".parse::<NodeValue>().is_err());
    assert!("Float(".parse::<NodeValue>().is_err());
    assert!("Float()".parse::<NodeValue>().is_err());
    assert!("Float(z)".parse::<NodeValue>().is_err());
    assert!("Float(0.1".parse::<NodeValue>().is_err());
    assert!("Foat".parse::<NodeValue>().is_err());
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TsNodeValue {
    pub value: NodeValue,
    pub ts: u32
}

impl TsNodeValue {
    //fn null() -> TsNodeValue { TsNodeValue { value: NodeValue::Null, ts: 0 } }
    pub fn new(v: NodeValue, ts: u32) -> TsNodeValue { TsNodeValue { value: v, ts: ts } }
    //pub fn from_string_and_ts(s: &str, ts: u32) -> Result<TsNodeValue, String> {
    //    s.parse().map(|nv| TsNodeValue { value: nv, ts: ts })
    //}
}


impl fmt::Display for TsNodeValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "TsNodeValue(value=", )
        .and_then(|_| self.value.fmt(f))
        .and_then(|_| write!(f, ", ts={})", self.ts))
    }
}


#[derive(Clone, Copy, Debug)]
pub enum DeviceOperation {
    Get,
    GetP(u8),
    Set(u8),
    SetP(u8, u8),
    SetFloat(f32),
    SetFloatP(u8, f32)
}

#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub struct OperationTarget {
    pub device_id: u8,
    pub instance_id: u8,
    pub command_class: u8
}

impl OperationTarget {
    pub fn new(device_id: u8, instance_id: u8, command_class: u8) -> OperationTarget {
        OperationTarget {
            device_id: device_id,
            instance_id: instance_id,
            command_class: command_class
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Event {
    NetUpdate(TsNodeValue),
    UserUpdate(TsNodeValue),
    UserGet,
    Timeout
}

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Event::NetUpdate(ref tnv) => write!(f, "NetUpdate({})", tnv),
            &Event::UserUpdate(ref tnv) => write!(f, "UserUpdate({})", tnv),
            &Event::UserGet => write!(f, "UserGet"),
            &Event::Timeout => write!(f, "Timeout")
        }
    }
}

/// WAN interface level command
pub enum WCmd {
    UserUpdate(String, TsNodeValue),
    UserGet(String),
    NetUpdate(String, TsNodeValue)
}

impl fmt::Display for WCmd {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &WCmd::NetUpdate(ref l, ref tnv) => write!(f, "NetUpdate({}, {})", l, tnv),
            &WCmd::UserUpdate(ref l, ref tnv) => write!(f, "UserUpdate({}, {})", l, tnv),
            &WCmd::UserGet(ref l) => write!(f, "UserGet({})", l)
         }
    }
}

pub struct FsmResult<'e> {
    output: Option<&'e Event>,
    output2: Option<Event>,
    timeout: Option<Duration>,
    new_state: Option<Box<FsmState>>,
    handled: bool
}

const UNHANDLED_FSM_RESULT: FsmResult<'static> = FsmResult {
    output: None,
    output2: None,
    timeout: None,
    new_state: None,
    handled: false
};

const NULL_FSM_RESULT: FsmResult<'static> = FsmResult {
    handled: true,
    ..UNHANDLED_FSM_RESULT
};



impl<'e> FsmResult<'e> {
    pub fn null() -> FsmResult<'e> { NULL_FSM_RESULT }
    pub fn unhandled() -> FsmResult<'e> { UNHANDLED_FSM_RESULT }
    pub fn for_state(st: Box<FsmState>) -> FsmResult<'e> {
        FsmResult { new_state: Some(st), ..NULL_FSM_RESULT }
    }
    pub fn for_event(e: &'e Event) -> FsmResult<'e> {
        FsmResult { output: Some(e), ..NULL_FSM_RESULT  }
    }
    pub fn for_timeout_d(t: Duration) -> FsmResult<'e> {
        FsmResult { timeout: Some(t), ..NULL_FSM_RESULT  }
    }
    pub fn for_timeout(secs: u64) -> FsmResult<'e> {
        FsmResult { timeout: Some(Duration::from_secs(secs)), ..NULL_FSM_RESULT  }
    }
    pub fn and_state(self, st: Box<FsmState>) -> FsmResult<'e> {
        FsmResult { new_state: Some(st), ..self }
    }
    pub fn and_event(self, e: &'e Event) -> FsmResult<'e> {
        FsmResult { output: Some(e), ..self }
    }
    pub fn and_event2(self, e: Event) -> FsmResult<'e> {
        FsmResult { output2: Some(e), ..self }
    }
    pub fn and_timeout_d(self, t: Duration) -> FsmResult<'e> {
        FsmResult { timeout: Some(t), ..self }
    }
    pub fn and_timeout(self, secs: u64) -> FsmResult<'e> {
        FsmResult { timeout: Some(Duration::from_secs(secs)), ..self }
    }

}

pub trait FsmState {
    fn handle<'e>(&self, e: &'e Event) -> FsmResult<'e>;
    fn get_timeout(&self) -> Option<Duration> { None }
    fn get_name(&self) -> &str;
}

struct NullFsmState;

impl FsmState for NullFsmState {
    fn handle<'e>(&self, _: &'e Event) -> FsmResult<'e> { FsmResult::null() }
    fn get_timeout(&self) -> Option<Duration> {
        None
    }
    fn get_name(&self) -> &str { "NullFsmState" }
}

struct TimerQ {
    q: BTreeSet<(Instant, usize)>,
    v: Vec<Option<Instant>>,
    now: Box<Fn() -> Instant>
}

impl TimerQ {
    fn new() -> TimerQ {
        TimerQ { q: BTreeSet::new(), v: Vec::new(), now: Box::new(Instant::now) }
    }

    fn add(&mut self) -> usize {
        self.v.push(None);
        self.v.len() - 1
    }

    fn schedule(&mut self, n: usize, t: Option<Duration>) {
        match self.v[n] {
            Some(ttime) => { self.q.remove(&(ttime, n)); () },
            None => ()
        }
        match t {
            Some(d) => {
                let ttime = (self.now)() + d;
                self.q.insert((ttime, n));
                self.v[n] = Some(ttime);
            },
            None => self.v[n] = None
        }
    }

    fn sleep_duration(&self) -> Option<Duration> {
        self.q.iter().next().map(|v| v.0 - (self.now)())
    }

    fn run(&mut self) -> Vec<usize> {
        let mut split = self.q.split_off(&((self.now)(), usize::max_value()));
        swap(&mut split, &mut self.q);
        let mut rv = Vec::new();
        for (_, n) in split {
            self.v[n] = None;
            rv.push(n);
        }
        rv
    }
}


#[test]
fn test_timerq() {

    let mut tq = TimerQ::new();
    let i0 = tq.add();
    let i1 = tq.add();
    let i2 = tq.add();
    let i3 = tq.add();
    let i4 = tq.add();
    assert_eq!(i0, 0);
    assert_eq!(i1, 1);
    assert_eq!(i2, 2);

    let t0 = Instant::now();
    let t1 = t0 + Duration::new(1, 0);
    //let t2 = t0 + Duration::new(2, 0);
    //let t3 = t0 + Duration::new(3, 0);
    //let t4 = t0 + Duration::new(4, 0);
    let t5 = t0 + Duration::new(5, 0);


    tq.now = Box::new(move || t0);
    tq.schedule(i2, Some(Duration::new(1, 0)));
    tq.schedule(i1, Some(Duration::new(5, 0)));
    tq.schedule(i0, Some(Duration::new(3, 0)));
    tq.schedule(i4, Some(Duration::new(2, 0)));
    tq.schedule(i3, Some(Duration::new(4, 0)));

    assert_eq!(tq.q.len(), 5);
    //println!("{:?}", tq.q);
    //println!("{:?}", tq.v);

    tq.now = Box::new(move || t1);
    let r = tq.run();
    assert_eq!(r, vec![2]);
    assert_eq!(tq.q.len(), 4);
    assert_eq!(tq.v[i2], None);
    //println!("{:?}", tq.q);
    //println!("{:?}", tq.v);

    tq.schedule(i1, Some(Duration::new(5, 0)));
    assert_eq!(tq.q.len(), 4);
    assert_ne!(tq.v[i1], None);

    tq.now = Box::new(move || t5);
    let v = tq.run();
    let mut r = HashSet::new();
    for n in v { r.insert(n); };
    assert_eq!(r, hash_set![0, 3, 4]);
    assert_eq!(tq.q.len(), 1);
    assert_eq!(tq.v[i1], tq.q.iter().next().map(|v| v.0));
    assert_eq!(tq.v[i0], None);
    assert_ne!(tq.v[i1], None);
    assert_eq!(tq.v[i2], None);
    assert_eq!(tq.v[i3], None);
    assert_eq!(tq.v[i4], None);
}

pub struct Universe<'t> {
    tq: TimerQ,
    el_states: Vec<Box<FsmState + 't>>
}

pub struct UniverseMetaData {
    index: HashMap<String, usize>,
    names: Vec<String>,
    links: Vec<HashSet<usize>>,
    max_countdown: usize
}

impl UniverseMetaData{
    fn new(max_countdown: usize) -> UniverseMetaData {
        UniverseMetaData {
            index: HashMap::new(),
            names: Vec::new(),
            links: Vec::new(),
            max_countdown: max_countdown
        }
    }
    ///Safely gets element name
    fn safe_get_element_name(&self, n: usize) -> &str {
        if n < self.names.len() { &self.names[n] } else { "???" }
    }
}

pub struct UniverseBuilder<'t> {
    u: Universe<'t>,
    md: UniverseMetaData
}

impl<'t> UniverseBuilder<'t> {
    pub fn new(max_countdown: usize) -> UniverseBuilder<'t> {
        UniverseBuilder { u: Universe::new(), md: UniverseMetaData::new(max_countdown) }
    }

    pub fn result(self) -> (Universe<'t>, UniverseMetaData) { (self.u, self.md) }

    fn introduce_element(&mut self, el: &str) -> usize {
        let ei = self.md.index.entry(el.to_string()).or_insert(self.u.el_states.len());
        if *ei == self.u.el_states.len() {
            self.u.el_states.push(Box::new(NullFsmState));
            self.u.tq.add();
            self.md.names.push(el.to_string());
            self.md.links.push(HashSet::new());
        }
        *ei
    }

    pub fn set_element_state(&mut self, el: &str, value: Box<FsmState + 't>) {
        let ei =  self.introduce_element(el);
        self.u.el_states[ei] = value;
    }

    pub fn add_link(&mut self, from: &str, to: &str) {
        let fe = self.introduce_element(from);
        let te = self.introduce_element(to);
        self.md.links[fe].insert(te);
    }
}

impl<'t> Universe<'t> {
    fn new() -> Universe<'t> {
        Universe { el_states: Vec::new(), tq: TimerQ::new() }
    }

    pub fn process_event(&mut self, md: &UniverseMetaData, el_id: &String, event: &Event) {
        match md.index.get(el_id) {
            Some(i) => self.process_event_by_index(md, *i, md.max_countdown, event),
            _ => error!("process_event: element not found: '{}'", el_id)
        }
    }

    pub fn run_timers(&mut self, md: &UniverseMetaData) {
        let v = self.tq.run();
        for i in v {
            self.process_event_by_index(&md, i, 1/*suppress propagation*/, &Event::Timeout)
        }
    }

    pub fn get_delay(&self, delay_min: Duration, delay_max: Duration) -> Duration {
        match self.tq.sleep_duration() {
            Some(d) => max(min(d, delay_max), delay_min),
            None => delay_max
        }
    }

    fn process_event_by_index(
        &mut self,
        md: &UniverseMetaData,
        element: usize,
        countdown: usize,
        event: &Event) {
        if countdown <= 0 {
            error!("Countdown=0 at element #{} ('{}'), event {:?} (graph cycle?); propagation stopped",
                element,
                md.safe_get_element_name(element),
                event
            );
            return;
        }

        use log::LogLevel::Debug;

        let istate = if log_enabled!(Debug) {
            self.el_states[element].get_name().to_string()
        } else {
            String::new()
        };

        let FsmResult { output: o, output2: o2, timeout: to, new_state: ns, handled: h } =
            self.el_states[element].handle(event);

        match ns {
            Some(st) => self.el_states[element] = st,
            _ => ()
        }

        debug!("{}:{}/{}=>{:?}/{}{}",
            md.safe_get_element_name(element),
            istate,
            event,
            o,
            self.el_states[element].get_name(),
            if h { "" } else { " <UNHANDLED>" }
        );

        for e in o {
            for ie in &md.links[element] {
                self.process_event_by_index(md, *ie, countdown - 1, e);
            }
        }

        for e in o2 {
            for ie in &md.links[element] {
                self.process_event_by_index(md, *ie, countdown - 1, &e);
            }
        }

        let st = to.or_else(||self.el_states[element].get_timeout());
        self.tq.schedule(element, st);
    }

}

