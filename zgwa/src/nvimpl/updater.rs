use nv::*;
use z_client::*;
use zgwlib::Result;

//Entry points

pub fn initial_readonly<'t>(ot: OperationTarget, ps: &'t ZClient)
    -> Box<FsmState + 't> {
    Box::new(Updater::new(ot, ps, c_none))
}

pub fn initial_thermostat_setpoint_heating<'t>(ot: OperationTarget, ps: &'t ZClient)
    -> Box<FsmState + 't> {
    Box::new(Updater::new(ot, ps, c_thermostat_setpoint_heating))
}

pub fn initial_switch_binary<'t>(ot: OperationTarget, ps: &'t ZClient)
    -> Box<FsmState + 't> {
    Box::new(Updater::new(ot, ps, c_switch_binary))
}

//------------------------------------------------------------------------------------------------
type Converter = fn(&NodeValue) -> Option<DeviceOperation>;

fn c_none(_value: &NodeValue) -> Option<DeviceOperation> {
    None
}

fn c_switch_binary(value: &NodeValue) -> Option<DeviceOperation> {
    match value {
        &NodeValue::Bool(v) => Some(DeviceOperation::Set(if v { 255 } else { 0 })),
        _ => None
    }
}

fn c_thermostat_setpoint_heating(value: &NodeValue) -> Option<DeviceOperation> {
    match value {
        &NodeValue::Float(v) => Some(DeviceOperation::SetFloatP(1, v as f32)),
        _ => None
    }
}

struct Updater<'t> {
    ot: OperationTarget,
    ps: &'t ZClient,
    uf: Converter
}

impl<'t> Updater<'t> {
    fn new(ot: OperationTarget, ps: &'t ZClient, c: Converter) -> Updater<'t> {
        Updater { ot: ot, ps: ps, uf: c }
    }
}

impl<'t> FsmState for Updater<'t> {
    fn handle<'e>(&self, e: &'e Event) -> FsmResult<'e> {
        let r: Result<bool> = match e {
            &Event::UserUpdate(TsNodeValue { value: ref v, ts: _ }) =>
                match (self.uf)(v) {
                    Some(op) => self.ps.exec(self.ot, op),
                    None => Err(::Error::gen(&format!("Invalid NV for updater: {}", v)))
                }.map(|_| true),
            &Event::UserGet =>
                self.ps.exec(self.ot, DeviceOperation::Get).map(|_| true),
            _ =>
                Ok(false)
        };
        //schedule low timeout here, to receive updates quickly
        r.map(|handled| if handled { FsmResult::for_timeout(5) } else { FsmResult::unhandled() })
        .unwrap_or_else(|e| { error!("Updater: {}", e); FsmResult::null() })

        /*match r {
            Ok(true) =>
                //schedule low timeout here, to receive updates quickly.
                FsmResult::for_timeout(5),
            Ok(false) => FsmResult::unhandled(),
            Err(e) => { error!("Updater: {}", e); FsmResult::null() }
        }*/
    }
    fn get_name(&self) -> &str { "-" }
}
