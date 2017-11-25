mod updater;
mod monitor;
mod pusher;

use nv::*;
use z_client::*;
use tree::TreeNode;
use std::sync::mpsc::Sender;

struct Cx<'t> {
    root: TreeNode,
    universe_builder: UniverseBuilder<'t>,
    ps: &'t ZClient,
    w: Sender<WCmd>
}

pub type ZEnvironment<'t> = ((Universe<'t>, UniverseMetaData), TreeNode);


#[derive(Serialize, Deserialize)]
pub enum UpdaterT {
    Readonly,
    ThermostatSetpointHeating,
    SwitchBinary
}

#[derive(Serialize, Deserialize)]
pub enum ElementState {
    Monitor { },
    Pusher { target: String },
    Updater { t: UpdaterT, ot: OperationTarget }
}

#[derive(Serialize, Deserialize)]
pub enum ConfigCommand {
    WireMonitoring { name: String, trigger_mp: String, ot: OperationTarget },
    MountTrigger { trigger_mp: String, monitor: String },
    Link { from: String, to: String },
    State { name: String, state: ElementState }
}


impl<'t> Cx<'t> {
    pub fn new(ps: &'t ZClient, w: Sender<WCmd>) -> Cx<'t> {
        Cx {
            root: TreeNode::root(),
            universe_builder: UniverseBuilder::new(64),
            ps: ps,
            w: w
        }
    }

    fn wire_monitoring(&mut self, name: &str, trigger_mp: &str, ot: OperationTarget) {
        let u = format!("{}.updater", name);
        let m = format!("{}.monitor", name);
        let p = format!("{}.pusher", name);

        self.universe_builder.set_element_state(&u, updater::initial_readonly(ot, self.ps));
        self.universe_builder.set_element_state(&m, monitor::initial());
        self.universe_builder.set_element_state(&p, pusher::initial(name, self.w.clone()));
        self.root.mount_trigger(trigger_mp, &m);
        self.universe_builder.add_link(&m, &u);
        self.universe_builder.add_link(&m, &p);
    }

    fn state_f<'q>(&self, state: ElementState) -> Box<FsmState + 'q> where 't : 'q {
        use self::ElementState::*;
        use self::UpdaterT::*;
        match state {
            Monitor { } =>
                monitor::initial(),
            Pusher { target } =>
                pusher::initial(&target, self.w.clone()),
            Updater { t: Readonly, ot } =>
                updater::initial_readonly(ot, self.ps),
            Updater { t: ThermostatSetpointHeating, ot } =>
                updater::initial_thermostat_setpoint_heating(ot, self.ps),
            Updater { t: SwitchBinary, ot } =>
                updater::initial_switch_binary(ot, self.ps)
        }
    }

    pub fn apply_command(&mut self, cc: ConfigCommand) {
        use self::ConfigCommand::*;
        match cc {
            WireMonitoring { name, trigger_mp, ot } =>
                self.wire_monitoring(&name, &trigger_mp, ot),
            MountTrigger { trigger_mp, monitor } =>
                self.root.mount_trigger(&trigger_mp, &monitor),
            Link { from, to } =>
                self.universe_builder.add_link(&from, &to),
            State { name, state } => {
                let st = self.state_f(state);
                self.universe_builder.set_element_state(&name, st)
            }
        }
    }

    pub fn result(self) -> ZEnvironment<'t> {
        (self.universe_builder.result(), self.root)
    }
}

pub fn configure_commands<'t, I>(ccs: I, ps: &'t ZClient, w: Sender<WCmd>)
                                 -> ZEnvironment<'t>
    where I: Iterator<Item=ConfigCommand>
{
    let mut cx = Cx::new(ps, w);
    for cc in ccs { cx.apply_command(cc); }
    cx.result()
}

pub type DeviceConfig = Vec<ConfigCommand>;

pub fn configure<'t>(ccs: DeviceConfig, ps: &'t ZClient, w: Sender<WCmd>)
                     -> ZEnvironment<'t> {
    configure_commands(ccs.into_iter(), ps, w)
}
