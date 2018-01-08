extern crate serde_json;

use std::collections::HashMap;
use std::io::Read;

use self::serde_json::{Value, Map};

use nv::NodeValue;
pub use interim_tree::InterimTreeNode;
use zgwlib::Result;

type Object = Map<String, Value>;
type VPM = HashMap<String, fn(&Value) -> Option<NodeValue>>;

pub struct JsonParser {
    vpm: VPM
}

impl JsonParser {
    pub fn new() -> JsonParser {
        let mut value_parsers: VPM  = HashMap::new();
        value_parsers.insert("int".to_owned(), vp_int);
        value_parsers.insert("int[]".to_owned(), vp_int_array);
        value_parsers.insert("string[]".to_owned(), vp_string_array);
        value_parsers.insert("binary".to_owned(), vp_binary);
        value_parsers.insert("float".to_owned(), vp_float);
        value_parsers.insert("string".to_owned(), vp_string);
        value_parsers.insert("bool".to_owned(), vp_bool);
        value_parsers.insert("empty".to_owned(), vp_empty);
        JsonParser { vpm: value_parsers }
    }

    fn parse_tree(&self, data: &Value) -> Vec<InterimTreeNode> {
        fn parse_update(object: &Object, vpm: &VPM) -> Vec<InterimTreeNode> {
            //get type early. needed to convert value appropriately
            let vtype = object.get("type").and_then(|v| match v {
                &Value::String(ref value) => vpm.get(value.as_str()),
                _ => None
            });

            object.iter().map(|kv| match kv {
                (&ref key, &Value::String(ref value)) if *key == "name" => InterimTreeNode::Name(value.clone()),
                (&ref key, &Value::String(ref value)) if *key == "type" => InterimTreeNode::Type(value.clone()),
                (&ref key, &ref json) if *key == "value" =>
                    match vtype.and_then(|f| f(json)) {
                        Some(nv) => InterimTreeNode::Value(nv),
                        None => InterimTreeNode::Unknown(key.clone(), format!("unhandled_value: {}", json))
                    },
                (&ref key, &Value::Object(ref o)) => InterimTreeNode::Compound(key.clone(), parse_update(o, vpm)),
                (&ref key, &Value::Number(ref value)) if *key == "updateTime" && value.is_u64() => InterimTreeNode::UpdateTime(value.as_u64().unwrap() as u32),
                (&ref key, &Value::Number(ref value)) if *key == "invalidateTime" && value.is_u64() => InterimTreeNode::InvalidateTime(value.as_u64().unwrap() as u32),
                (&ref key, &ref value) => InterimTreeNode::Unknown(key.clone(), format!("{}", value))
            }).collect()
        }

        match data {
            &Value::Object(ref update_object) => parse_update(update_object, &self.vpm),
            &ref other => vec![InterimTreeNode::Unknown("Update batch not an object".to_string(), format!("{}", other))]
        }
    }

    pub fn parse_update(&self, r: &mut Read) -> Result<Vec<InterimTreeNode>> {
        serde_json::from_reader(r).map(|data| self.parse_tree(&data))
            .map_err(|e| ::Error::other("json parse error", e))
    }
}

fn vp_int(j: &Value) -> Option<NodeValue> { match j {
    &Value::Number(ref value) => value.as_i64().map(|v| NodeValue::Int(v)),
    _ => None
} }
fn vp_float(j: &Value) -> Option<NodeValue> { match j {
    &Value::Number(ref value) => value.as_f64().map(|v| NodeValue::Float(v)),
    _ => None
} }
fn vp_string(j: &Value) -> Option<NodeValue> { match j {
    &Value::String(ref value) => Some(NodeValue::String(value.clone())),
    _ => None
} }
fn vp_bool(j: &Value) -> Option<NodeValue> { match j {
    &Value::Bool(value) => Some(NodeValue::Bool(value)),
    _ => None
} }
fn vp_int_array(j: &Value) -> Option<NodeValue> { match j {
    &Value::Array(ref v) => Some(NodeValue::IntArray({
            let mut r = Vec::new();
            for i in v {
                match match i {
                    &Value::Number(ref value) => value.as_i64(),
                    _ => return None
                } {
                    Some(j) => r.push(j),
                    _ => return None
                }

            }
            r
        })),
    _ => None
} }
fn vp_string_array(j: &Value) -> Option<NodeValue> { match j {
    &Value::Array(ref v) => Some(NodeValue::StringArray({
        let mut rv: Vec<String> = Vec::new();
        for i in v { match *i {
            Value::String(ref value) => rv.push(value.clone()),
            _ => return None
        } };
        rv
    })),
    _ => None
} }
fn vp_binary(j: &Value) -> Option<NodeValue> { match j {
    &Value::Array(ref v) => Some(NodeValue::Binary({
        let mut r = Vec::new();
        for i in v {
            match match i {
                &Value::Number(ref value) => value.as_u64(),
                _ => return None
            } {
                Some(j) if j < 256 => r.push(j as u8),
                _ => return None
            }
        }
        r
    })),
    _ => None
} }
fn vp_empty(j: &Value) -> Option<NodeValue> { match j {
    &Value::Null => Some(NodeValue::Null),
    _ => None
} }



#[test]
fn test_vp_int_array() {
    let a = serde_json::from_str(r#"[0,1,2,3]"#).unwrap();
    let vec = vec![0,1,2,3];
    assert_eq!(vp_int_array(&a), Some(NodeValue::IntArray(vec)));

    let b: Value = serde_json::from_str(r#"[0,1,"x",3]"#).unwrap();
    assert_eq!(vp_int_array(&b), None);

}
#[test]
fn test_vp_binary() {
    let a = serde_json::from_str(r#"[0,1,2,255]"#).unwrap();
    let va = vec![0,1,2,255];
    assert_eq!(vp_binary(&a), Some(NodeValue::Binary(va)));

    let b: Value = serde_json::from_str(r#"[0,1,"x",3]"#).unwrap();
    assert_eq!(vp_binary(&b), None);

    let c: Value = serde_json::from_str(r#"[0,1,0,256]"#).unwrap();
    assert_eq!(vp_binary(&c), None);
}
