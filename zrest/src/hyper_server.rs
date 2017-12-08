use futures::{Future, Stream};
use tokio_core::reactor::Core;

use hyper;
use hyper::{Request, Response, Body, Chunk, StatusCode, Uri, Method};
use hyper::header::{/*Authorization, Basic,*/ ContentType};
use mime::Mime;
use serde;
use serde_json;
use ::*;

use std::collections::hash_map::{HashMap, Entry};
use std::fmt::Debug;

/// Type of a variable carried in request-uri
#[derive(Debug, PartialEq, Eq, Hash)]
enum VarType {
    STRING,
    INT,
    FLOAT
}

/// Method + content-type of incoming data, if any
#[derive(Debug, PartialEq, Eq, Hash)]
enum MCT {
    GET,
    POST(Mime),
    PUT(Mime),
    DELETE
}

/// Condition for branching in the tree
#[derive(Debug, PartialEq, Eq, Hash)]
enum Condition {
    /// Matches constant string segment with implicit separator or terminator (ST) at the end
    /// If empty, matches exact ST.
    CSTR(String),
    /// Matches single non-ST character.
    CCHAR(char),
    /// Matches ST.
    CST,
    /// Matches variable
    VAR(VarType),
    MCT(MCT)
}

impl<'t> From<&'t str> for Condition {
    fn from(s: &'t str) -> Self { Condition::CSTR(s.to_owned()) }
}


/// Tree action type
#[derive(Debug)]
enum Action<C> {
    NOP,
    SYNC_JSON_F0(Box<SyncJsonF0<C>>),
    SYNC_JSON_F1(Box<SyncJsonF1<C>>),
}

impl<C> Action<C> {
    fn f1<S>(s: S) -> Self where S: SyncJsonF1<C> + 'static { Action::SYNC_JSON_F1(Box::new(s)) }
}

/*
impl<C, S> From<S> for Action<C> where S: SyncJsonF1<C> {
    fn from(s: S) -> Self { Action::SYNC_JSON_F1(Box::new(s)) }
}
*/

/// Tree offset, or position, or address.
type Pos = usize;

/// Tree node type
#[derive(Debug)]
struct Node<C> {
    children: HashMap<Condition, Pos>,
    action: Action<C>
}

impl<C> Node<C> {
    #[inline]
    fn new() -> Node<C> { Node::create(HashMap::new()) }
    fn create(m: HashMap<Condition, Pos>) -> Node<C> {
        Node { children: m, action: Action::NOP }
    }
}

/// The value of a request-uri var
enum Val {
    STRING(String),
    INT(i64),
    FLOAT(f64)
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
    tree: Vec<Node<C>>
}

/// Splits s into one-char head and tail
fn split(s: &String) -> Option<(char, String)> {
    let mut ch = s.chars();
    let hopt = ch.next();
    hopt.map(|h|(h, ch.as_str().to_string()))
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
    fn new() -> Builder<C> { Builder { tree: vec![Node::new()] } }

    fn print(&self) {
        use std::iter::FromIterator;
        fn line<C>(tree: &Vec<Node<C>>, n: &Node<C>, pos: usize, depth: usize) where C: std::fmt::Debug {
            let head = format!("[{:#03}]{}",
                               pos,
                               String::from_iter(std::iter::repeat(' ').take(depth)));
            match &n.action {
                &Action::NOP => (),
                r => println!("{}!{:?}", head, r)
            }
            for (k, v) in &n.children {
                println!("{}{:?}->{}", head, k, v);
                line(tree, &tree[*v], *v, depth + 1);
            }
        }
        line(&self.tree, &self.tree[0], 0, 0);
    }

    fn mount(&mut self, cond: Vec<Condition>, a: Action<C>) {
        let a_offset = cond.into_iter().fold(
            0,
            |offset, cond| {
                let next = self.tree.len();
                let o = match self.tree[offset].children.entry(cond) {
                    Entry::Occupied(oe) => *oe.get(),
                    Entry::Vacant(ve) => *ve.insert(next)

                };
                if o == next { self.tree.push(Node::new()); }
                o
            }
        );
        match &mut self.tree[a_offset].action {
            r @ &mut Action::NOP => *r = a,
            _ => panic!("Conflicting actions at node {}", a_offset)
        }
    }

    fn expand(&mut self) {
        use std::mem::swap;

        /// Performs CSTR expansion step.
        /// n is the node to be expanded; free_pos is the first free position in the tree;
        /// returns new nodes to be added at free_pos, free_pos + 1, ...
        /// Each CSTR is converted into CCHAR with its head plus one new node with the tail
        /// (CSTR(s) -> t) ==> (CCHAR(head(s)) -> (CSTR(tail(s)) -> t))
        fn e_step<C>(n: &mut Node<C>, free_pos: Pos) -> Vec<Node<C>> {

            let mut ch = HashMap::new();
            swap(&mut ch, &mut n.children);

            let (mv, mut rd) = ch.into_iter().fold(
                (HashMap::<char, HashMap<Condition, Pos>>::new(), HashMap::<Condition, Pos>::new()),
                |(mut mv, mut rd), (cond, target)| {
                    match cond {
                        Condition::CSTR(s) =>
                            if let Some((c, rest)) = split(&s) {
                                assert_none!(mv.entry(c).or_insert_with(||HashMap::new()).insert(Condition::CSTR(rest), target));
                            } else {
                                assert_none!(rd.insert(Condition::CST, target));
                            },
                        c => {
                            assert_none!(rd.insert(c, target));
                        }
                    }
                    (mv, rd)
                }
            );

            let mut rv = Vec::new();

            for ((ch, mp), i) in mv.into_iter().zip(free_pos..) {
                assert_none!(rd.insert(Condition::CCHAR(ch), i));
                rv.push(Node::create(mp));
            }

            n.children = rd;
            rv
        }

        let mut done = Vec::new();
        let mut todo = Vec::new();
        swap(&mut self.tree, &mut todo);

        while !todo.is_empty() {
            let mut cur = Vec::new();
            swap(&mut todo, &mut cur);
            let fp0 = done.len() + cur.len();
            for rn in &mut cur {
                let fp = fp0 + todo.len();
                todo.append(&mut e_step(rn, fp))
            }
            done.append(&mut cur);
        }
        swap(&mut self.tree, &mut done);
    }



    fn run(&self, p: String) -> Option<&Action<C>> {
        let mut ch = p.chars();
        let mut pos = 0;
        loop {
            let n = &self.tree[pos];
            if let Some(c) = ch.next() {
                let cond = match c { '/' => Condition::CST, cc => Condition::CCHAR(cc) };
                if let Some(&target) = n.children.get(&cond) {
                    pos = target;
                } else {
                    break None;
                }
            } else {
                break Some(&n.action);
            }
        }
    }
}

mod url_scanner {
    use std::str::Chars;

    #[derive(Debug, Eq, PartialEq)]
    pub enum C { CH(char), ST }

    #[derive(Copy, Clone)]
    enum S { INIT, CH, ST, ERR, END }

    pub struct Scanner<'s> { ch: Chars<'s>, s: S }

    impl<'s> Scanner<'s> {
        pub fn new(url: &'s str) -> Scanner<'s> { Scanner{ ch: url.chars(), s: S::INIT } }
    }

    impl<'s> Iterator for Scanner<'s> {
        type Item = C;
        fn next(&mut self) -> Option<Self::Item> {
            loop {
                let (s1, rv) = match (self.s, self.ch.next()) {
                    (S::END, _) => break None,
                    (S::ERR, _) => break None,
                    //strip initial `/`
                    (S::INIT, Some('/')) => (S::CH, None),
                    (S::INIT, _) => (S::ERR, None),
                    //'/' in the middle
                    (_, Some('/')) => (S::ST, Some(C::ST)),
                    //ordinary char
                    (_, Some(c)) => (S::CH, Some(C::CH(c))),
                    //append '/' at the end, if not there
                    (S::ST, None) => (S::END, None),
                    (_, None) => (S::ST, Some(C::ST))
                };
                self.s = s1;
                if rv.is_some() { break rv; }
            }
        }
    }

    #[test]
    fn test_url_scanner() {
        fn v(p: &str) -> Vec<C> { Scanner::new(p).collect() }
        assert_eq!(v("/"), vec![C::ST]);
        assert_eq!(v("/a"), vec![C::CH('a'), C::ST]);
        assert_eq!(v("/a/"), vec![C::CH('a'), C::ST]);
        assert_eq!(v("/a/b"), vec![C::CH('a'), C::ST, C::CH('b'), C::ST]);
        assert_eq!(v("/a/b/"), vec![C::CH('a'), C::ST, C::CH('b'), C::ST]);
        assert_eq!(v("//"), vec![C::ST]);
    }

}

#[test]
fn test_rest_server_builder() {
    let mut b = Builder::<String>::new();
    b.mount(sconds!("s1", "s2"), Action::f1(Echo));
    //println!("{:?}", b);
    b.mount(sconds!("s1", "s2a"), Action::f1(Echo));
    b.expand();
    //println!("{:?}", b);
    //b.print();
    //println!("{:?}", b.run("s1/s2a/".to_string()));
    //println!("{:?}", b.run("s1/s2x/".to_string()));
    assert!(b.run("s1/s2a/".to_string()).is_some());
    assert!(b.run("s1/s2x/".to_string()).is_none());
}


enum RC {
    TRUTH,
    CHAR(char),
    VAR(VarType),
    MCT(MCT)
}

/// Runner node
enum RNode<C> {
    BRANCH { cond: RC, b_succees: Pos, b_failure: Pos },
    RUN(Action<C>)
}

impl<C> RNode<C> {
    fn offset_by(&mut self, off: usize) {
        match self {
            &mut RNode::BRANCH { cond: _, ref mut b_succees, ref mut b_failure } => {
                *b_succees += off;
                *b_failure += off;
            },
            _ => ()
        }
    }
}

struct RTree<C> {
    tree: Vec<RNode<C>>
}

impl<C> RTree<C> {
/*
    fn new(b: Builder<C>) {
        fn step(bt: &Vec<Node<C>>, bn: Node<C>) -> LinkedList<RNode<C>> {
            let Node { children, action } = bn;

            let (mv, rd) = children.into_iter().fold(
                (HashMap::<char, HashMap<Condition, Pos>>::new(), HashMap::<Condition, Pos>::new()),
                |(mut mv, mut rd), (cond, target)| {
                    match cond {
                        Condition::CSTR(s) =>
                            if let Some((c, rest)) = split(&s) {
                                assert_none!(mv.entry(c).or_insert_with(||HashMap::new()).insert(Condition::CSTR(rest), target));
                            } else {
                                assert_none!(rd.insert(Condition::CST, target));
                            },
                        c => {
                            assert_none!(rd.insert(c, target));
                        }
                    }
                    (mv, rd)
                }
            );


        }
    }
*/

    /*
    /// adds constants
    ///




    /// adds a constant to the tree. Arguments are  the constant value, a label to go to on
    /// success and a label to go to on failure. Returns entry label
    fn add_const(entry: Option<Pos>, c: String, b_success: Pos, b_failure: Pos) -> ZResult<Pos> {
        let mut chs = c.chars();
        let c0 = chs.next().ok_or(ZRError::EZR("Invalid empty constant into add_const"))?;
        match pos {
            Some(p) =>

        }

        unimplemented!()
    }
    */
}




//--------------------------------------------------------------------------------------------------

const PHRASE: &'static str = "Hello, World!";

struct Router;

use hyper::server::Service;
use hyper::header::{ContentLength};

impl Service for Router {
    // boilerplate hooking up hyper's server types
    type Request = Request;
    type Response = Response;
    type Error = hyper::Error;
    // The future representing the eventual Response your call will
    // resolve to. This can change to whatever Future you need.
    type Future = Box<Future<Item=Self::Response, Error=Self::Error>>;

    fn call(&self, _req: Request) -> Self::Future {
        // We're currently ignoring the Request
        // And returning an 'ok' Future, which means it's ready
        // immediately, and build a Response with the 'PHRASE' body.
        Box::new(futures::future::ok(
            Response::new()
                .with_header(ContentLength(PHRASE.len() as u64))
                .with_body(PHRASE)
        ))
    }
}

