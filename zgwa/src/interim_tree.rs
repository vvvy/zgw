use std::fmt;
use nv::NodeValue;

#[derive(Debug)]
pub enum InterimTreeNode {
    Name(String),
    Value(NodeValue),
    Type(String),
    UpdateTime(u32),
    InvalidateTime(u32),
    Compound(String, Vec<InterimTreeNode>),
    Unknown(String, String)
}

impl fmt::Display for InterimTreeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmtx(self, f, 0)
    }
}

fn fmtx(n: &InterimTreeNode, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
    write!(f, "{:.*}", pad, "                                 ").and_then(
    |_| match n {
        &InterimTreeNode::Name(ref v) => writeln!(f, "Name({})", v),
        &InterimTreeNode::Value(ref v) => writeln!(f, "Value({})", v),
        &InterimTreeNode::Type(ref v) => writeln!(f, "Type({})", v),
        &InterimTreeNode::UpdateTime(v) => writeln!(f, "UpdateTime({})", v),
        &InterimTreeNode::InvalidateTime(v) => writeln!(f, "InvalidateTime({})", v),
        &InterimTreeNode::Unknown(ref k, ref v) => writeln!(f, "Unknown({}, {})", k, v),
        &InterimTreeNode::Compound(ref k, ref v) =>
            v.iter().fold(
                writeln!(f, "Compound({})", k),
                |_, nn| { fmtx(nn, f, pad + 1) }
            )
    })
}
