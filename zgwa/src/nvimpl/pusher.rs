use nv::*;
use std::sync::mpsc::Sender;

pub fn initial<'t>(label: &str, sender: Sender<WCmd>) -> Box<FsmState + 't> {
    Box::new(Pusher { label: label.to_owned(), sender: sender })
}

struct Pusher {
    sender: Sender<WCmd>,
    label: String
}

impl FsmState for Pusher {
    fn handle<'e>(&self, e: &'e Event) -> FsmResult<'e> {
        match e {
            &Event::NetUpdate(ref tnv) => {
                let _ = self.sender.send(WCmd::NetUpdate(self.label.clone(), tnv.clone()));
                FsmResult::null()
            },
            _ =>
                FsmResult::unhandled()
        }

    }
    fn get_name(&self) -> &str { "-" }
}
