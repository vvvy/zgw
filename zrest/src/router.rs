use hyper::{Body, Chunk, Method};
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
enum VarType {
    Str,
    Int,
    Float
}

/// Method + content-type of incoming data, if any
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum MCT {
    GET,
    POST(Mime),
    PUT(Mime),
    DELETE
}

/// Condition for branching in the tree
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum Condition {
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


/// Tree action type
#[derive(Debug)]
enum Action<C> {
    NOP,
    SyncJsonF0(Box<SyncJsonF0<C>>),
    SyncJsonF1(Box<SyncJsonF1<C>>),
}

impl<C> Action<C> {
    fn f1<S>(s: S) -> Self where S: SyncJsonF1<C> + 'static { Action::SyncJsonF1(Box::new(s)) }
    fn f0<S>(s: S) -> Self where S: SyncJsonF0<C> + 'static { Action::SyncJsonF0(Box::new(s)) }
}

/// Builder tree node
#[derive(Debug)]
struct BNode<C> {
    children: HashMap<Condition, Box<BNode<C>>>,
    action: Action<C>
}

enum ChildrenAnalysisResult<C> {
    Empty,
    Strings(HashMap<String, Box<BNode<C>>>),
    Var(VarType, Box<BNode<C>>),
    MCTs(HashMap<MCT, Box<BNode<C>>>),
    Err(String)
}

use std::hash::Hash;
#[inline]
fn map_add<K: Eq + Hash, V>(mut m: HashMap<K, V>, k: K, v: V) -> HashMap<K, V> {
    m.insert(k, v);
    m
}

impl<C> ChildrenAnalysisResult<C> {
    fn analyze_children(src: HashMap<Condition, Box<BNode<C>>>) -> ChildrenAnalysisResult<C> {
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




impl<C> BNode<C> {
    #[inline]
    fn new() -> BNode<C> { BNode::create(HashMap::new()) }
    fn create(m: HashMap<Condition, Box<BNode<C>>>) -> BNode<C> {
        BNode { children: m, action: Action::NOP }
    }
}

/// The value of a request-uri var
enum Val {
    Str(String),
    Int(i64),
    Float(f64)
}

/// A synchronous operation with no JSON input (typically GET).
trait SyncJsonF0<C>: Debug {
    /// Returns response body from the context reference and request-uri values
    fn apply(&self, cx: &mut C, vals: &[Val]) -> Body;
}

/// A synchronous operation with JSON input
trait SyncJsonF1<C>: Debug {
    /// Returns response body from context reference, request-uri values, and request chunk
    fn apply(&self, cx: &mut C, vals: &[Val], ch: Chunk) -> Body;
}

#[derive(Debug)]
struct Echo;

impl<C> SyncJsonF1<C> for Echo {
    fn apply(&self, cx: &mut C, vals: &[Val], ch: Chunk) -> Body {
        Body::from(ch)
    }
}

#[derive(Debug)]
struct Builder<C> {
    root: BNode<C>
}


macro_rules! assert_none(
{ $value:expr } => { assert!(($value).is_none()) }
);

macro_rules! sconds(
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

impl<C> Builder<C> where C: std::fmt::Debug {
    fn new() -> Builder<C> { Builder { root: BNode::new() } }

    fn print(&self) {
        use std::iter::FromIterator;
        fn line<C>(n: &BNode<C>, depth: usize) where C: std::fmt::Debug {
            let head = format!("{}",
                               String::from_iter(std::iter::repeat(' ').take(depth)));
            match &n.action {
                &Action::NOP => (),
                r => println!("{}!{:?}", head, r)
            }
            for (k, v) in &n.children {
                println!("{}{:?}->", head, k);
                line(v, depth + 1);
            }
        }
        line(&self.root, 0);
    }

    fn mount(&mut self, condition: Vec<Condition>, a: Action<C>) {
        fn step<'t, C: 't>(n: &'t mut BNode<C>, condition: &[Condition], a: Action<C>) {
            match condition.first() {
                Some(cond) =>
                    match n.children.entry(cond.clone()) {
                        Entry::Occupied(mut oe) => step(oe.get_mut(), &condition[1..], a),
                        Entry::Vacant(ve) => step(ve.insert(Box::new(BNode::new())), &condition[1..], a)
                    },
                None => match &mut n.action {
                    r @ &mut Action::NOP => *r = a,
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
    b.mount(sconds!("s1", "s2"), Action::f1(Echo));
    //println!("{:?}", b);
    b.mount(sconds!("s1", "s2a"), Action::f1(Echo));
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

/// Runner node
#[derive(Debug)]
enum RNode<C> {
    Branch { cond: RCond, b_success: Label, b_failure: Label },
    Run(Action<C>)
}

enum CodeChunkBody<C> {
    Branches(Vec<(RCond, Label)>, Label),
    Action(Action<C>)
}

struct CodeChunk<C> {
    label: Label,
    body: CodeChunkBody<C>
}

use std::collections::LinkedList;

struct RTreeBuildContext<C> {
    next_label: Label,
    result: Vec<CodeChunk<C>>,
    err: Option<String>
}

impl<C> RTreeBuildContext<C> {
    const FAILURE_POS: Label = 0;

    fn new() -> RTreeBuildContext<C> {
        let mut r = Vec::new();
        r.push(CodeChunk { label: Self::FAILURE_POS, body: CodeChunkBody::Action(Action::f1(Echo)) });
        RTreeBuildContext {
            next_label: r.len(),
            result: r,
            err: None
        }
    }

    fn create(b: Builder<C>) -> (RTreeBuildContext<C>, Label) {
        let mut cx = RTreeBuildContext::new();
        let entry = cx.emit(b);
        (cx, entry)
    }

    fn add_chunk(&mut self, body: CodeChunkBody<C>) -> Label {
        let l = self.next_label;
        self.next_label +=1;
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

    fn emit(&mut self, b: Builder<C>) -> Label {
        self.bnode(b.root)
    }

    /// emit action node
    /// returns chunk label
    fn anode(&mut self, action: Action<C>) -> Option<Label> {
        match action {
            Action::NOP => None,
            other => Some(self.add_chunk(CodeChunkBody::Action(other)))
        }
    }

    /// emits builder node
    fn bnode(&mut self, n: BNode<C>) -> Label {
        let BNode { children, action } = n;
        use std::iter::FromIterator;

        #[inline]
        fn s2cl(s: String) ->  LinkedList<char> { s.chars().collect() }

        let anode_l = self.anode(action);
        let exit_step = self.add_branch_opt(
            anode_l.map(|apos| (RCond::EOT, apos)),
            Self::FAILURE_POS
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
                Self::FAILURE_POS
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

type Pos = usize;

struct RTree<C> {
    tree: Vec<RNode<C>>,
    init: Pos
}

impl<C> RTree<C> {
    fn build(b: Builder<C>) -> ZRResult<RTree<C>> {
        let (RTreeBuildContext { next_label: label_count, result: chunks, err }, entry) =
            RTreeBuildContext::create(b);

        //label table. for each label (@offset), holds its offset within the resulting tree
        let mut label_table: Vec<Option<Pos>> =
            std::iter::repeat(None).take(label_count).collect();
        let mut tree: Vec<RNode<C>> = Vec::new();

        //pass one: emit chunks into the three, registering actual label positions into label_table
        for CodeChunk { label: chunk_label, body: chunk_body } in chunks {
            let tpos = tree.len();
            //fix the label's position and complain if already fixed
            let mut swapped = Some(tpos);
            std::mem::swap(&mut swapped, &mut label_table[chunk_label]);
            assert_none!(swapped);

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

        //pass two: replace labels with actual offsets
        for n in &mut tree {
            match n {
                &mut RNode::Branch { cond: _, b_success: ref mut s, b_failure: ref mut f  } => {
                    *s = label_table[*s].unwrap();
                    *f = label_table[*f].unwrap();
                },
                _ => ()
            }
        }

        Ok(RTree { tree: tree, init: label_table[entry].unwrap() })
    }



    /*
    fn print(&self) {
        fn step<C>(n: &RNode<C>, pos: Pos, indent: usize) {
            let head = format!("[{:#03}]{:ind$}", pos, "", ind=indent);
            match n {
                &RNode::Branch { cond, b_success, b_faiure }
            }

        }
    }
    */

    fn run(&self, s: String) -> ZRResult<&Action<C>> {
        let mut sc = Scanner::new(&s);

        enum O {
            SG(Pos),
            G(Pos),
            S
        };

        let mut cp = self.init;
        let mut cv = sc.next();
        let mut vars: Vec<Vec<char>> = Vec::new();

        loop {
            match &self.tree[cp] {
                &RNode::Run(ref a) => break Ok(a),
                &RNode::Branch { cond: ref cond, b_success, b_failure } => {
                    if cv.is_none() { match sc.get_error() {
                        Some(serr) => break Err(ZRError::from(serr.clone())),
                        _ => ()
                    } }

                    let res = match (&cv, cond) {
                        (&Some(E::CH(ch1)), &RCond::Ch(ch2)) if ch1 == ch2 => O::SG(b_success),
                        (&Some(E::ST), &RCond::Sep) => O::SG(b_success),
                        (&None, &RCond::EOT) => O::SG(b_success),
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
