use std::collections::{VecDeque, HashMap};
use std::hash::Hash;
use std::usize;
use std::cmp::{min, max};

struct Q<T> {
    sn_head: usize,
    q: VecDeque<T>
}

#[derive(Debug, PartialEq)]
pub struct Feed<T>(pub Vec<T>, pub usize);

impl<T> Q<T> {
    fn new() -> Q<T> { Q { sn_head: 0, q: VecDeque::new() } }
    fn push(&mut self, e: T) {
        self.q.push_back(e);
    }

    fn sn_head(&self) -> usize { self.sn_head }
    fn len(&self) -> usize { self.q.len() }
    //fn is_empty(&self) -> bool { self.q.is_empty() }

    ///Acknowledges reception of and thus discards all items having SNs less than sn_next
    fn ack(&mut self, sn_next: usize) {
        let n = min(
            max((sn_next as isize) - (self.sn_head as isize), 0) as usize,
            self.q.len()
        );

        self.sn_head += n;
        self.q.drain(0..n);
    }

    /// Feeds at most count_max items into the 0.0 field of the returned Feed.
    /// sn_next to the values fed is returned in 0.1 field of the Feed.
    /// field 2 is Boolean indicator if there are more messages (beyond those fed)
    fn get_feed(&self, count_max: usize) -> (Feed<T>, bool) where T: Clone {
        let vs: Vec<T> = self.q.iter().take(count_max).cloned().collect();
        let len = vs.len();
        (Feed(vs, self.sn_head + len), len < self.q.len())
    }
}

pub struct QRecv {
    sn_acked: usize
}

impl QRecv {
    pub fn new() -> QRecv { QRecv { sn_acked: 0 }}

    pub fn acked(&self) -> usize {
        self.sn_acked
    }

    pub fn feed_through<T, E, F>(&mut self, feed: Feed<T>, f: F) -> Result<usize, E>
        where F: Fn(Vec<T>)-> Result<(), E> {
        if self.sn_acked + feed.0.len() != feed.1 {
            //sync loss
            self.sn_acked = 0;
            Ok(self.sn_acked)
        } else {
            match f(feed.0) {
                Ok(_) => {
                    self.sn_acked = feed.1;
                    Ok(self.sn_acked)
                },
                Err(err) => Err(err)
            }
        }
    }
}

#[test]
fn test_q() {
    let mut vd = Q::<u8>::new();
    for i in 10..16 {
        vd.push(i);
    }
    assert_eq!(vd.q, vec_deque![10, 11, 12, 13, 14, 15]);
    assert_eq!(vd.sn_head, 0);

    assert_eq!(vd.get_feed(3), (Feed(vec![10, 11, 12], 3), true));
    assert_eq!(vd.get_feed(7), (Feed(vec![10, 11, 12, 13, 14, 15], 6), false));

    assert_eq!(vd.sn_head, 0);

    vd.ack(4);
    assert_eq!(vd.q, vec_deque![14, 15]);
    assert_eq!(vd.sn_head, 4);

    vd.ack(4);
    assert_eq!(vd.q, vec_deque![14, 15]);
    assert_eq!(vd.sn_head, 4);

    vd.ack(10);
    assert!(vd.q.is_empty());
    assert_eq!(vd.sn_head, 6);
}

///Snapshotted queue
pub struct SQ<K, T> {
    s: HashMap<K, T>,
    q: Q<T>,
    sn_next_t: usize    //next
}



impl<K, T> SQ<K, T> where T: AsRef<K> + Clone, K: Hash + Eq + Clone {
    pub fn new() -> SQ<K, T> {
        SQ { s: HashMap::new(), q: Q::new(), sn_next_t: 0 }
    }

    /// Pushes the value into both the queue and the snapshot.
    /// Resets the queue if it is more than twice as long as the snapshot.
    pub fn push(&mut self, e: T) {
        let k: K = e.as_ref().clone();
        self.s.insert(k, e.clone());

        if self.q.len() >= 2 * self.s.len() {
            self.reset_queue();
        } else {
            self.q.push(e);
        }
    }

    /// Reset the queue and fill it from the snapshot
    fn reset_queue(&mut self) {
        self.q = Q::new();
        let v: Vec<&T> = self.s.values().collect();
        for e in v { self.q.push(e.clone()); }
    }

    /// Acknowledges reception of and thus discards all items having SNs less than sn_next.
    /// Detects loss of sync and resets the queue if detected.
    /// Returns in sync flag.
    pub fn ack(&mut self, sn_next: usize) -> bool {
        if self.q.sn_head() <= sn_next && sn_next <= self.sn_next_t {
            //in sync
            self.q.ack(sn_next);
            true
        } else {
            //out-of-sync
            self.reset_queue();
            self.sn_next_t = 0;
            false
        }
    }

    /// Feeds at most count_max items into the 0th field of the returned Feed.
    /// sn_next to the values fed is returned in 1st field.
    /// Indicator... (see above)
    /// No queue state is aletered. "Maximum sn_next transmitted" is adjusted.
    pub fn get_feed(&mut self, count_max: usize) -> (Feed<T>, bool) {
        let rv = self.q.get_feed(count_max);
        self.sn_next_t = max(self.sn_next_t, (rv.0).1);
        rv
    }

    //pub fn is_empty(&self) -> bool { self.q.is_empty() }
    pub fn q_size(&self) -> usize { self.q.len() }
}



#[test]
fn test_sq() {
    #[derive(PartialEq, Eq, Clone, Debug, PartialOrd, Ord)]
    struct T(u8, &'static str, u8); //(ts, key, value)

    impl AsRef<&'static str> for T {
        fn as_ref(&self) -> & &'static str { &self.1 }
    }

    let mut sq = SQ::<&str, T>::new();

    // Filling in
    sq.push(T(0, "A", 1));
    sq.push(T(0, "B", 10));
    sq.push(T(0, "C", 100));
    assert_eq!(sq.q.q, vec_deque![T(0, "A", 1), T(0, "B", 10), T(0, "C", 100)]);
    assert_eq!(sq.s, hash_map!["A" => T(0, "A", 1), "B" =>  T(0, "B", 10), "C" => T(0, "C", 100)]);

    // Replacing 2 out of 3 snapshot values
    sq.push(T(1, "A", 2));
    sq.push(T(2, "B", 11));
    assert_eq!(sq.q.q, vec_deque![T(0, "A", 1), T(0, "B", 10), T(0, "C", 100),
        T(1, "A", 2), T(2, "B", 11)
    ]);
    assert_eq!(sq.s, hash_map!["A" => T(1, "A", 2), "B" =>  T(2, "B", 11), "C" => T(0, "C", 100)]);

    assert_eq!(
        sq.get_feed(5),
        (Feed(
            vec![T(0, "A", 1), T(0, "B", 10), T(0, "C", 100),
                T(1, "A", 2), T(2, "B", 11)],
            5), false)
    );
    assert_eq!(sq.sn_next_t, 5);

    // Feed 2 more items and have the queue reset
    sq.push(T(3, "C", 101));
    sq.push(T(4, "B", 12));
    assert_eq!(sq.s, hash_map!["A" => T(1, "A", 2), "B" =>  T(4, "B", 12), "C" => T(3, "C", 101)]);
    let mut r = Vec::from(sq.q.q.clone());
    r.sort();
    assert_eq!(r, vec![T(1, "A", 2), T(3, "C", 101), T(4, "B", 12)]);
    assert_eq!(sq.q.sn_head(), 0);

    assert!(sq.ack(2));
    assert_eq!(sq.q.sn_head(), 2);
    assert_eq!(sq.q.q.len(), 1);

    // standard no-error run
    let mut sq = SQ::<&str, T>::new();
    sq.push(T(0, "A", 1));
    sq.push(T(0, "B", 10));
    sq.push(T(0, "C", 100));
    sq.push(T(1, "A", 2));
    sq.push(T(2, "B", 11));
    sq.push(T(3, "C", 101));
    assert_eq!(sq.q.len(), 6);
    assert_eq!(sq.q.sn_head(), 0);

    assert_eq!(sq.get_feed(2), (Feed(vec![T(0, "A", 1), T(0, "B", 10)], 2), true));
    assert!(sq.ack(2));
    assert_eq!(sq.q.len(), 4);
    assert_eq!(sq.q.sn_head(), 2);

    assert_eq!(sq.get_feed(2), (Feed(vec![T(0, "C", 100), T(1, "A", 2)], 4), true));
    assert!(sq.ack(4));
    assert_eq!(sq.q.len(), 2);
    assert_eq!(sq.q.sn_head(), 4);

    sq.push(T(4, "B", 13));

    assert_eq!(sq.get_feed(2), (Feed(vec![T(2, "B", 11), T(3, "C", 101)], 6), true));
    assert!(sq.ack(6));
    assert_eq!(sq.q.len(), 1);
    assert_eq!(sq.q.sn_head(), 6);

    assert_eq!(sq.get_feed(2), (Feed(vec![T(4, "B", 13)], 7), false));
    assert!(sq.ack(7));
    assert_eq!(sq.q.len(), 0);
    assert_eq!(sq.q.sn_head(), 7);

}