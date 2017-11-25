use nv::*;

///One and the only public entry point

pub fn initial<'t>() -> Box<FsmState + 't> {
    Box::new(MonitorNull)
}

//-------------------------------------------------------------------------------------------------
struct MonitorNull;

impl FsmState for MonitorNull {
    fn handle<'e>(&self, e: &'e Event) -> FsmResult<'e> {
        match e {
            &Event::NetUpdate(ref nv) =>
                FsmResult::for_event(e).and_state(Box::new(MonitorIdle::new(nv.clone()))),
            &Event::UserGet =>
                FsmResult::for_event(e),   //TODO: timeout logics
            _ =>
                FsmResult::unhandled()
        }
    }
    fn get_name(&self) -> &str { "Idle" }
}

//-------------------------------------------------------------------------------------------------

struct MonitorIdle {
    #[allow(dead_code)]
    value: TsNodeValue
}

impl MonitorIdle {
    fn new(value: TsNodeValue) -> MonitorIdle {
        MonitorIdle { value: value }
    }
}

impl FsmState for MonitorIdle {
    fn handle<'e>(&self, e: &'e Event) -> FsmResult<'e> {
        match e {
            &Event::NetUpdate(ref nv) =>    //todo: stale update logics
                FsmResult::for_event(e).and_state(Box::new(MonitorIdle::new(nv.clone()))),
            &Event::UserGet =>
                FsmResult::for_event(e),   //TODO: timeout logics
            _ =>
                FsmResult::unhandled()
        }
    }
    fn get_name(&self) -> &str { "Idle" }
}

//-------------------------------------------------------------------------------------------------