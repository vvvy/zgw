use mime::Mime;
use ::*;

use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;

macro_rules! hash_map(
    { $($key:expr => $value:expr),+ } => {
        {
            let mut m = ::std::collections::HashMap::new();
            $(
                m.insert($key, $value);
            )+
            m
        }
    };
);


/// Type of a variable carried in request-uri
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum VarType {
    Str,
    Int,
    Float
}

/// Method + content-type of incoming data, if any
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MCT {
    GET,
    POST(Mime),
    PUT(Mime),
    DELETE
}

/// Condition for branching in the tree
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Condition {
    /// Matches constant string segment with implicit separator or terminator (ST) at the end
    /// If empty, matches exact ST.
    CStr(String),
    /// Matches variable
    Var(VarType),
    /// matches input content-type
    MCT(MCT)
}

impl<'t> From<&'t str> for Condition {
     fn from(s: &'t str) -> Self { Condition::CStr(s.to_owned()) }
}

impl From<VarType> for Condition {
    fn from(vt: VarType) -> Self { Condition::Var(vt) }
}

/// Builder tree node
#[derive(Debug)]
struct BNode<A> {
    children: HashMap<Condition, Box<BNode<A>>>,
    action: Option<A>
}

enum ChildrenAnalysisResult<A> {
    Empty,
    Strings(HashMap<String, Box<BNode<A>>>),
    Var(VarType, Box<BNode<A>>),
    MCTs(HashMap<MCT, Box<BNode<A>>>),
    Err(String)
}

use std::hash::Hash;
#[inline]
fn map_add<K: Eq + Hash, V>(mut m: HashMap<K, V>, k: K, v: V) -> HashMap<K, V> {
    m.insert(k, v);
    m
}

impl<A> ChildrenAnalysisResult<A> {
    fn analyze_children(src: HashMap<Condition, Box<BNode<A>>>) -> ChildrenAnalysisResult<A> {
        src.into_iter().fold(
            ChildrenAnalysisResult::Empty,
            |car, (cond, n)| match (car, cond) {
                (ChildrenAnalysisResult::Empty, Condition::CStr(s)) =>
                    ChildrenAnalysisResult::Strings(hash_map!(s => n)),

                (ChildrenAnalysisResult::Empty, Condition::Var(vt)) =>
                    ChildrenAnalysisResult::Var(vt, n),

                (ChildrenAnalysisResult::Empty, Condition::MCT(mct)) =>
                    ChildrenAnalysisResult::MCTs(hash_map!(mct => n)),

                (ChildrenAnalysisResult::Strings(m), Condition::CStr(s)) =>
                    ChildrenAnalysisResult::Strings(map_add(m, s, n)),

                (ChildrenAnalysisResult::MCTs(m), Condition::MCT(mct)) =>
                    ChildrenAnalysisResult::MCTs(map_add(m, mct, n)),

                (ChildrenAnalysisResult::Err(s), _) => ChildrenAnalysisResult::Err(s),
                _ => ChildrenAnalysisResult::Err("invalid combination of node ops".to_string())
            }
        )
    }
}

impl<A> BNode<A> {
    #[inline]
    fn new() -> BNode<A> { BNode::_create(HashMap::new()) }
    fn _create(m: HashMap<Condition, Box<BNode<A>>>) -> BNode<A> {
        BNode { children: m, action: None }
    }
}

/// The value of a request-uri var
#[derive(Debug)]
pub enum Val {
    Str(String),
    Int(i64),
    Float(f64)
}

#[derive(Debug)]
pub struct Builder<A> {
    root: BNode<A>
}


macro_rules! assert_none(
{ $value:expr } => { assert!(($value).is_none()) }
);

#[macro_export]
macro_rules! route_expr(
    { $($value:expr),+ } => {
        {
            let mut v = ::std::vec::Vec::new();
            $(
                v.push(Condition::from($value));
            )+
            v
        }
    };
);

impl<A: Debug> Builder<A> {
    fn print(&self) {
        use std::iter::FromIterator;
        fn line<A>(n: &BNode<A>, depth: usize) where A: Debug {
            let head = format!("{}",
                               String::from_iter(std::iter::repeat(' ').take(depth)));
            match &n.action {
                &None => (),
                &Some(ref r) => println!("{}!{:?}", head, r)
            }
            for (k, v) in &n.children {
                println!("{}{:?}->", head, k);
                line(v, depth + 1);
            }
        }
        line(&self.root, 0);
    }
}

impl<A> Builder<A> {
    pub fn new() -> Builder<A> { Builder { root: BNode::new() } }

    pub fn mount(&mut self, condition: Vec<Condition>, a: A) {
        fn step<'t, A: 't>(n: &'t mut BNode<A>, condition: &[Condition], a: A) {
            match condition.first() {
                Some(cond) =>
                    match n.children.entry(cond.clone()) {
                        Entry::Occupied(mut oe) => step(oe.get_mut(), &condition[1..], a),
                        Entry::Vacant(ve) => step(ve.insert(Box::new(BNode::new())), &condition[1..], a)
                    },
                None => match &mut n.action {
                    r @ &mut None => *r = Some(a),
                    // TODO proper panic
                    _ => panic!("Conflicting actions at node ?")
                }
            }
        }
        step(&mut self.root, &condition, a);
    }
}

#[test]
fn test_rest_server_builder() {
    let mut b = Builder::<String>::new();
    b.mount(route_expr!("s1", "s2"), "<SUCCESS>".to_string());
    //println!("{:?}", b);
    b.mount(route_expr!("s1", "s2a"), "<SUCCESS>".to_string());
    println!("------------------------------------------");
    b.print();
    println!("------------------------------------------");
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum RCond {
    Ch(char),
    Sep,
    EOT,
    Var(VarType),
    MCT(MCT)
}

type Label = usize;

#[inline]
fn mirror_label(l: Label) -> Label { usize::max_value() - l }

/// Runner node
#[derive(Debug)]
enum RNode<A> {
    Branch { cond: RCond, b_success: Label, b_failure: Label },
    Run(A)
}

enum CodeChunkBody<A> {
    Branches(Vec<(RCond, Label)>, Label),
    Action(A)
}

struct CodeChunk<A> {
    label: Label,
    body: CodeChunkBody<A>
}


/// allocates and returns virtual labels.
/// Virtual labels grow from `usize::max_value()` down
struct LabelAllocator {
    next_n: Label
}

impl LabelAllocator {
    fn new() -> LabelAllocator { LabelAllocator { next_n: 0 }}
    fn get_next(&mut self) -> Label {
        let l = self.next_n;
        self.next_n += 1;
        mirror_label(l)
    }
}

/// holds mapping of virtual labels to physical offsets
struct LabelTable {
    t: Vec<Option<Label>>,
    threshold: Label
}

impl LabelTable {
    fn new(la: LabelAllocator) -> LabelTable {
        LabelTable {
            t: std::iter::repeat(None).take(la.next_n).collect(),
            threshold: mirror_label(la.next_n)
        }
    }

    /// fix the label's position and complain if already fixed
    fn set_pos(&mut self, label: Label, pos: Label) {
        let mut swapped = Some(pos);
        std::mem::swap(&mut swapped, &mut self.t[mirror_label(label)]);
        assert_none!(swapped);
    }

    /// if the label is virtual, convert it to physical.
    fn materialize(&self, l: Label) -> ZRResult<Label> {
        if l > self.threshold {
            self.t[mirror_label(l)].ok_or(ZRError::EZR(format!("internal error: missing label ({})", l)))
        } else {
            Ok(l)
        }
    }

    /// check that vlabels and physical code do not overlap
    fn is_valid(&self, code_size: usize) -> bool {
        code_size <= self.threshold

    }

}

use std::collections::LinkedList;

struct RTreeBuildContext<A> {
    la: LabelAllocator,
    result: Vec<CodeChunk<A>>,
    l_failure: Label,
    err: Option<String>
}

impl<A> RTreeBuildContext<A> {
    fn new(a_failure: A) -> RTreeBuildContext<A> {
        let mut r = Vec::new();
        let mut la = LabelAllocator::new();
        let l_failure = la.get_next();

        r.push(CodeChunk { label: l_failure, body: CodeChunkBody::Action(a_failure) });
        RTreeBuildContext {
            la: la,
            result: r,
            l_failure: l_failure,
            err: None
        }
    }

    fn create(b: Builder<A>, a_failure: A) -> (RTreeBuildContext<A>, Label) {
        let mut cx = RTreeBuildContext::new(a_failure);
        let entry = cx.emit(b);
        (cx, entry)
    }

    fn add_chunk(&mut self, body: CodeChunkBody<A>) -> Label {
        let l = self.la.get_next();
        self.result.push(CodeChunk { label: l, body: body });
        l
    }

    fn add_branches(&mut self, v: Vec<(RCond, Label)>, default: Label) -> Label {
        if v.is_empty() {
            default
        } else {
            self.add_chunk(CodeChunkBody::Branches(v, default))
        }
    }

    fn add_branch_opt(&mut self, v: Option<(RCond, Label)>, default: Label) -> Label {
        match v {
            None => default,
            Some(cl) => self.add_chunk(CodeChunkBody::Branches(vec![cl], default))
        }
    }

    fn emit(&mut self, b: Builder<A>) -> Label {
        let e0 = self.bnode(b.root);
        //insert match for initial /
        let f = self.l_failure;
        self.add_branch_opt(Some((RCond::Sep, e0)), f)
    }

    /// emit action node
    /// returns chunk label
    fn anode(&mut self, action: Option<A>) -> Option<Label> {
        action.map(|a| self.add_chunk(CodeChunkBody::Action(a)))
    }

    /// emits builder node
    fn bnode(&mut self, n: BNode<A>) -> Label {
        let BNode { children, action } = n;
        use std::iter::FromIterator;

        #[inline]
        fn s2cl(s: String) ->  LinkedList<char> { s.chars().collect() }

        let anode_l = self.anode(action);
        let f = self.l_failure;
        let exit_step = self.add_branch_opt(
            anode_l.map(|apos| (RCond::EOT, apos)),
            f
        );

        match ChildrenAnalysisResult::analyze_children(children) {
            ChildrenAnalysisResult::Empty =>
                exit_step,
            ChildrenAnalysisResult::Strings(hm) => {
                let m = HashMap::from_iter(hm.into_iter().map(|(s, n)| (s2cl(s), self.bnode(*n))));
                self.string_map(m, exit_step)
            },
            ChildrenAnalysisResult::Var(vt, n) => {
                let l = self.bnode(*n);
                self.add_branch_opt(Some((RCond::Var(vt), l)), exit_step)
            },
            ChildrenAnalysisResult::MCTs(mctm) => {
                let v = mctm.into_iter().map(|(mct, n)|(RCond::MCT(mct), self.bnode(*n))).collect();
                self.add_branches(v,exit_step)
            },
            ChildrenAnalysisResult::Err(desc) => {
                self.err = Some(desc);
                self.l_failure
            }
        }
    }

    fn string_map(&mut self, m: HashMap<LinkedList<char>, Label>, default: Label)-> Label {
        #[inline]
        fn split(mut s: LinkedList<char>) -> Option<(char, LinkedList<char>)> {
            let v = s.pop_front();
            v.map(|ch| (ch, s))
        }

        let (chars, st) = m.into_iter().fold(
            (HashMap::<char, HashMap<LinkedList<char>, Label>>::new(), None as Option<Label>),
            |(mut m, mut st), (s, t)| {
                if let Some((head, tail)) = split(s) {
                    let rv = m.entry(head)
                        .or_insert_with(|| HashMap::new())
                        .insert(tail, t);
                    assert_none!(rv);
                } else {
                    assert_none!(st);
                    st = Some(t);
                }
                (m, st)
            }
        );

        let mut w: Vec<(RCond, Label)> = chars.into_iter().map(|(ch, submap)|
            (RCond::Ch(ch), self.string_map(submap, default))
        ).collect();

        //append ST branch, if any
        for pos in st {
            w.push((RCond::Sep, pos));
        }

        self.add_branches(w, default)
    }
}

use uri_scanner::*;

pub struct RTree<A> {
    tree: Vec<RNode<A>>,
    init: Label
}

impl<A> RTree<A> where A: Debug {
    fn print(&self)  {
        for (pos, n) in self.tree.iter().enumerate() {
            print!("{}[{:#03}]  ", if pos == self.init { "*" } else { " " }, pos);
            match n {
                &RNode::Branch { ref cond, ref b_success, ref b_failure } =>
                    println!("{:?} {} {}", cond, b_success, b_failure),
                &RNode::Run(ref a) =>
                    println!("! {:?}", a)
            }
        }
    }
}

impl<A> RTree<A> {
    pub fn build(b: Builder<A>, a_failure: A) -> ZRResult<RTree<A>> {
        let (RTreeBuildContext { la, result: chunks, err, l_failure: _ }, entry) =
            RTreeBuildContext::create(b, a_failure);

        if let Some(m) = err {
            return Err(ZRError::EZR(m));
        }

        //label table. for each label (@offset), holds its offset within the resulting tree
        let mut label_table = LabelTable::new(la);
        let mut tree: Vec<RNode<A>> = Vec::new();

        //pass one: emit chunks into the three, registering actual label positions into label_table
        for CodeChunk { label: chunk_label, body: chunk_body } in chunks {
            let tpos = tree.len();
            label_table.set_pos(chunk_label, tpos);

            //emit chunk's body
            tree.append(&mut match chunk_body {
                CodeChunkBody::Branches(v, default) => if v.is_empty() {  vec![] } else {
                    let elses =
                        (tpos + 1 .. tpos + v.len()).into_iter().chain(std::iter::once(default));
                    v.into_iter().zip(elses)
                        .map(|((cond, b_ok), b_fail)| RNode::Branch { cond: cond, b_success: b_ok, b_failure: b_fail})
                        .collect()
                },
                CodeChunkBody::Action(a) =>
                    vec![RNode::Run(a)]
            });

        }

        if !label_table.is_valid(tree.len()) {
            return Err(ZRError::EZR(format!("code too big ({})", tree.len())));
        }

        //pass two: replace labels with actual offsets
        for n in &mut tree {
            match n {
                &mut RNode::Branch { cond: _, b_success: ref mut s, b_failure: ref mut f  } => {
                    *s = label_table.materialize(*s)?;
                    *f = label_table.materialize(*f)?;
                },
                _ => ()
            }
        }

        Ok(RTree { tree: tree, init: label_table.materialize(entry)? })
    }

    pub fn run<'t, 'q>(&'t self, s: &'q str) -> ZRResult<(&'t A, Vec<Val>)> {
        let mut sc = Scanner::new(&s);

        enum O {
            SG(Label),
            G(Label),
            S
        };

        let mut cp = self.init;
        let mut cv = sc.next();
        let mut vars: Vec<Val> = Vec::new();
        let mut svar = String::new();

        loop {
            match &self.tree[cp] {
                &RNode::Run(ref a) => break Ok((a, vars)),
                &RNode::Branch { ref cond, b_success, b_failure } => {
                    let res = match (&cv, cond) {
                        (&Some(E::ERR(serr)), _) => break Err(ZRError::from(serr.clone())),
                        (&Some(E::CH(ch1)), &RCond::Ch(ch2)) if ch1 == ch2 => O::SG(b_success),
                        (&Some(E::ST), &RCond::Sep) => O::SG(b_success),
                        (&None, &RCond::EOT) => O::SG(b_success),
                        (&Some(E::CH(ch)), &RCond::Var(_)) => {
                            svar.push(ch);
                            O::S
                        },
                        (&Some(E::ST), &RCond::Var(vt)) => {
                            let res = match vt {
                                VarType::Str => Ok(Val::Str(svar)),
                                VarType::Float => std::str::FromStr::from_str(&svar).map(|v| Val::Float(v)).map_err(|_|()),
                                VarType::Int => std::str::FromStr::from_str(&svar).map(|v| Val::Int(v)).map_err(|_|())
                            };
                            svar = String::new();
                            match res {
                                Ok(v) => { vars.push(v); O::SG(b_success) },
                                Err(_) => { O::SG(b_failure) },
                            }
                        },
                        _ => O::G(b_failure)
                    };
                    match res {
                        O::SG(pos) => { cv = sc.next(); cp = pos; },
                        O::G(pos) => { cp = pos; },
                        O::S => { cv = sc.next(); }
                    }
                }
            }
        }
    }
}

///-------------------------------------------------------------------------------------------------
/// parses uri pattern from string
///

pub fn parse_uri_pattern(pattern: &str) -> Vec<Condition> {
    pattern.split('/').filter(|s|!s.is_empty()).map(|s|
        match s {
            "$" | "$s" => Condition::Var(VarType::Str),
            "$i" => Condition::Var(VarType::Int),
            "$f" => Condition::Var(VarType::Float),
            oth => Condition::CStr(oth.to_string())
        }
    ).collect()
}


///-------------------------------------------------------------------------------------------------
/// this solely exists for the tests to work.
/// USE WITH CAUTION (as a Float doesn't compare equal to itself).
#[cfg(test)]
impl PartialEq for Val {
    fn eq(&self, other: &Val) -> bool {
        match (self, other) {
            (&Val::Str(ref s1), &Val::Str(ref s2)) => s1 == s2,
            (&Val::Int(i1), &Val::Int(i2)) => i1 == i2,
            _ => false
        }
    }
}
#[cfg(test)]
impl Eq for Val { }
///-------------------------------------------------------------------------------------------------

#[cfg(test)]
macro_rules! s(
    { $value:expr } => { ($value).to_string() }
);

#[cfg(test)]
macro_rules! check_route(
    { $tree:expr, $input:expr => $result:expr, [ $($var:expr),* ] } => {
        assert_eq!(($tree).run($input).unwrap(), (&s!($result), vec![$(Val::Str(s!($var)),)*]));
    };
);

#[test]
fn test_router_a() {
    let mut b = Builder::<String>::new();
    b.mount(route_expr!("s1", "s2a"), s!("<s2a>"));
    b.mount(route_expr!("s1", "s3", VarType::Str), "<s3>".to_string());
    let t = RTree::build(b, "<FAIL>".to_string()).unwrap();
    //t.print();
    check_route!(t, "/s1/s2a" => "<s2a>", []);
    check_route!(t, "/s1/s3/abcde/" => "<s3>", ["abcde"])
}

#[test]
fn test_router_b() {
    let mut b = Builder::<String>::new();
    b.mount(route_expr!("v1", "user"), s!("<user>"));
    b.mount(route_expr!("v1", "user", VarType::Str), s!("<user+$>"));
    b.mount(route_expr!("v1", "user", VarType::Str, "data"), s!("<user+$+data>"));
    b.mount(route_expr!("v1", "n", VarType::Int, "s", VarType::Str), s!("<n+$+$>"));
    let t = RTree::build(b, s!("<FAIL>")).unwrap();
    check_route!(t, "/v1/user/abcde/data" => "<user+$+data>", ["abcde"]);
    check_route!(t, "/v1/user/" => "<user>", []);
    check_route!(t, "/v1/user/abcde" => "<user+$>", ["abcde"]);
    assert_eq!(t.run("/v1/n/123/s/zaq").unwrap(), (&s!("<n+$+$>"), vec![Val::Int(123), Val::Str(s!("zaq"))]));
}