use std::fmt;

use interim_tree::InterimTreeNode;
//use std::result::Result;

use nv::{NodeValue, TsNodeValue};

#[derive(Clone, Debug)]
pub struct NodeData {
    node_name: Option<String>,
    node_type: Option<String>,
    node_value: Option<NodeValue>,
    update_time: Option<u32>,
    invalidate_time: Option<u32>
}

impl NodeData {
    pub fn to_ts_node_value(self) -> Option<TsNodeValue> {
        match (self.node_value, self.update_time) {
            (Some(nv), Some(ut)) => Some(TsNodeValue { value: nv, ts: ut }),
            _ => None
        }
    }
}


use std::collections::BTreeMap;

//ident compound
type IdC = Vec<String>;

pub struct Trigger {
    target: String
}

pub trait CoTrigger {
    fn fire<'t>(&'t mut self, trigger: &'t Trigger, node_data: &'t NodeData);
}

#[allow(dead_code)]
pub struct NopCoTrigger;

impl CoTrigger for NopCoTrigger {
    fn fire<'t>(&'t mut self, _: &'t Trigger, _: &'t NodeData) { }
}

#[allow(dead_code)]
pub fn nop_co_trigger() -> NopCoTrigger {
    NopCoTrigger
}

pub struct TreeNode {
    data: NodeData,
    trigger: Option<Trigger>,
    children: BTreeMap<String, TreeNode>
}

impl TreeNode {
    fn ap<'t>(&'t self, ct: &'t mut CoTrigger) {
        match self.trigger {
            Some(ref t) => ct.fire(t, &self.data),
            None => ()
        }
    }
}

impl fmt::Display for TreeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmtx(self, f, 0)
    }
}

fn fmtx(n: &TreeNode, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
    fn ok() -> fmt::Result { Result::Ok(()) }
    let r = ok()
    .and_then(|_| n.data.node_name.as_ref().map(|v| write!(f, "name={} ", v)).unwrap_or(ok()))
    .and_then(|_| n.data.node_type.as_ref().map(|v| write!(f, "type={} ", v)).unwrap_or(ok()))
    .and_then(|_| n.data.node_value.as_ref().map(|v| write!(f, "value={} ", v)).unwrap_or(ok()))
    .and_then(|_| n.data.update_time.as_ref().map(|v| write!(f, "ut={} ", v)).unwrap_or(ok()))
    .and_then(|_| n.data.invalidate_time.as_ref().map(|v| write!(f, "it={} ", v)).unwrap_or(ok()))
    ;
    n.children.iter().fold(r, |a, i|{
        let (k, v) = i;
        a
        .and_then(|_| writeln!(f, ""))
        .and_then(|_| write!(f, "{:.*}", pad, "                                 "))
        .and_then(|_| write!(f, "{}: ", k))
        .and_then(|_| fmtx(v, f, pad + 1))
        })
}

impl TreeNode {
    pub fn empty() -> TreeNode {
        TreeNode {
            data : NodeData {
                node_name: None,
                node_type: None,
                node_value: None,
                update_time: None,
                invalidate_time: None
            },
            trigger: None,
            children: BTreeMap::new()
        }
    }

    pub fn root() -> TreeNode {
        let mut n = TreeNode::empty();
        n.data.update_time = Some(0);
        n
    }

    fn shift_mut(&mut self, id: String) -> &mut TreeNode {
        self.children.entry(id).or_insert_with(|| TreeNode::empty())
    }

    fn shift_multi_mut(&mut self, idc: IdC) -> &mut TreeNode {
        idc.into_iter().fold(self, |v, id| v.shift_mut(id))
    }

    pub fn merge_multi<'t>(&'t mut self, itn: Vec<InterimTreeNode>, ct: &'t mut CoTrigger) {
        for n in itn {
            self.merge(n, ct);
        }
    }
    fn merge<'t>(&'t mut self, itn: InterimTreeNode, ct: &'t mut CoTrigger) {
        match itn {
            InterimTreeNode::Name(v) => { self.data.node_name = Some(v); self.ap(ct) },
            InterimTreeNode::Type(v) => { self.data.node_type = Some(v); self.ap(ct) },
            InterimTreeNode::Value(v) => { self.data.node_value = Some(v); self.ap(ct) },
            InterimTreeNode::UpdateTime(v) => { self.data.update_time = Some(v); self.ap(ct) },
            InterimTreeNode::InvalidateTime(v) => { self.data.invalidate_time = Some(v); self.ap(ct) },
            InterimTreeNode::Compound(s, v) => self.shift_multi_mut(split_ident(&s)).merge_multi(v, ct),
            _ => ()
        }
    }
    
    pub fn get_update_time<'r>(&'r self) -> &'r Option<u32> {
        &self.data.update_time
    }

    pub fn mount_trigger(&mut self, path: &str, target: &str) {
        let tn = self.shift_multi_mut(split_ident2(path));
        tn.trigger = Some(Trigger { target : target.to_string() });
    }
}

pub struct UpdateCollector {
    pub updates: BTreeMap<String, NodeData>
}

pub fn new_update_collector() -> UpdateCollector {
    UpdateCollector { updates: BTreeMap::new() }
}

impl CoTrigger for UpdateCollector {
    fn fire<'t>(&'t mut self, trigger: &'t Trigger, node_data: &'t NodeData) {
        self.updates.insert(trigger.target.clone(), (*node_data).clone());
    }
}

fn split_ident(ident: &String) -> IdC {
    ident.split('.').map(|v| v.to_string()).collect()
}

fn split_ident2(ident: &str) -> IdC {
    ident.split('.').map(|v| v.to_string()).collect()
}
