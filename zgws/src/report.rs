use std::sync::mpsc::{Receiver};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::io::*;
use std::fs::File;
use std::fs::OpenOptions;
use std::result::Result as SResult;

extern crate chrono;

use nv::TsNodeValue;
use w::UWEvt;


pub struct Reporter {
    values: HashMap<String, TsNodeValue>,
    n_logged_total: u32,
    n_logged_current: u32,
    n_updates: u32,
    last_ts: u32,
    log: PathBuf,
    status: PathBuf,
    heartbeat: PathBuf,
    records_per_logfile: u32,
    n_logs: i8
}

#[derive(Debug, PartialEq)]
enum RotCmd {
    Remove(PathBuf),
    Move {from: PathBuf, to: PathBuf}
}

/// Returns a list of instructions on what to do with log rotation
/// base is main logfile path. Archive log paths are derived by changing extension to "1", "2", ...
/// count is number of archive logs to maintain. If 0, the main logfile gets truncated when full.
/// If <0, the rotation is disabled.
/// exists is file existence checker

fn rotate_program(base: &PathBuf, count: i8, exists: fn(&PathBuf) -> bool) -> Vec<RotCmd> {
    if count < 0 { return vec![] }

    let ff = |n: i8| if n == 0 {
        base.clone()
    } else {
        base.with_extension(format!("{}", n))
    };

    //find upper bound to rotate to. Generally, this should always yield to None (i.e., no holes).
    //it isn't None if there was an error during last moves, and there is a 'hole'
    let ub = (0..(count + 1)).find(|p| !exists(&ff(*p)));

    let mut rv = if ub.is_none() { vec![RotCmd::Remove(ff(count))] } else { vec![] };

    for n in (0..ub.unwrap_or(count)).rev() {
        rv.push(RotCmd::Move {from: ff(n), to: ff(n + 1)})
    }

    rv
}

#[test]
fn test_rotate_program() {
    use self::RotCmd::*;
    use std::ffi::OsStr;

    fn p(s: &str) -> PathBuf { PathBuf::from(s) }
    let base = p("./log");

    fn e_all(_: &PathBuf) -> bool { true }
    fn e_but_0(p: &PathBuf) -> bool { p.extension().is_some() }
    fn e_but_1(p: &PathBuf) -> bool { p.extension() != Some(OsStr::new("1")) }
    fn e_but_2(p: &PathBuf) -> bool { p.extension() != Some(OsStr::new("2")) }
    fn e_but_3(p: &PathBuf) -> bool { p.extension() != Some(OsStr::new("3")) }

    assert!(!e_but_0(&p("./log")));

    assert!(!e_but_2(&p("./log.2")));
    assert!(e_but_2(&p("./log")));
    assert!(e_but_2(&p("./log.1")));

    assert_eq!(
        rotate_program(&base, 3, e_all),
        vec![
            Remove(p("./log.3")),
            Move { from: p("./log.2"), to: p("./log.3") },
            Move { from: p("./log.1"), to: p("./log.2") },
            Move { from: p("./log"), to: p("./log.1") }
        ]
    );

    assert_eq!(
        rotate_program(&base, 3, e_but_1),
        vec![
            Move { from: p("./log"), to: p("./log.1") }
        ]
    );

    assert_eq!(
        rotate_program(&base, 3, e_but_2),
        vec![
            Move { from: p("./log.1"), to: p("./log.2") },
            Move { from: p("./log"), to: p("./log.1") }
        ]
    );

    assert_eq!(
        rotate_program(&base, 3, e_but_3),
        vec![
            Move { from: p("./log.2"), to: p("./log.3") },
            Move { from: p("./log.1"), to: p("./log.2") },
            Move { from: p("./log"), to: p("./log.1") }
        ]
    );

    assert_eq!(
        rotate_program(&base, 3, e_but_0),
        vec![]
    );
}


fn rotate_logs(base: &PathBuf, count: i8) -> SResult<(), Error> {
    use std::fs::{remove_file, rename};

    fn exists(p: &PathBuf) -> bool { p.exists() }

    let cmds = rotate_program(base, count, exists);

    cmds.iter().fold(Ok(()), |r, cmd| r.and_then(|_| match cmd {
        &RotCmd::Remove(ref p) => remove_file(p),
        &RotCmd::Move { ref from, ref to } => rename(from, to)
    }))
}


impl Reporter {
    pub fn new(dir: PathBuf, records_per_logfile: u32, n_logs: i8) -> Reporter {
        let mut rv = Reporter {
            values: HashMap::new(),
            n_logged_current: 0,
            n_logged_total: 0,
            n_updates: 0,
            last_ts: 0,
            log: dir.join(Path::new("log")),
            status: dir.join(Path::new("status")),
            heartbeat: dir.join(Path::new("heartbeat")),
            records_per_logfile,
            n_logs
        };

        //try to recover heartbeat file values
        let contents = File::open(&rv.heartbeat)
            .and_then(|mut f| { let mut s = String::new(); f.read_to_string(&mut s).map(|_| s)});

        use std::str::SplitWhitespace;

        fn ru32(sw: &mut SplitWhitespace) -> SResult<u32, String> {
            sw.next()
                .map(|s| s.parse::<u32>().map_err(|e| format!("parse err {}", e)))
                .unwrap_or(Err("missing field".to_owned()))
        }

        let _ = contents
            .map_err(|e| format!("i/o error: {}", e))
            .map(|l| {
                let mut sw = l.split_whitespace();
                let f0 = ru32(&mut sw);
                let f1 = ru32(&mut sw);
                let f2 = ru32(&mut sw);
                let f3 = ru32(&mut sw);
                match (f0, f1, f2, f3) {
                    (Ok(n_logged_current), Ok(n_logged_total), Ok(n_updates), Ok(last_ts)) => {
                        rv.n_logged_current = n_logged_current;
                        rv.n_logged_total = n_logged_total;
                        rv.n_updates = n_updates;
                        rv.last_ts = last_ts;
                        Ok(())
                    },
                    _ => Err("?".to_owned())
                }
            });
        debug!("Reporter initialized, n_logged_current={}, n_logged_total={}, n_updates={}, last_ts={}",
               rv.n_logged_current, rv.n_logged_total, rv.n_updates, rv.last_ts);
        rv
    }

    fn handle_update(&mut self, id: String, nv: TsNodeValue) {
        use self::chrono::prelude::*;
        let local: DateTime<Local> = Local::now();
        let local_s = local.format("%Y-%m-%d %H:%M:%S").to_string();

        self.n_logged_total += 1;
        self.n_logged_current += 1;

        if self.n_logged_current > self.records_per_logfile {
            if rotate_logs(&self.log, self.n_logs).is_ok() {
                self.n_logged_current = 1;
            }
        }

        //Write log
        let _ = OpenOptions::new().create(true).append(true).open(&self.log)
            .map(|f| BufWriter::new(f))
            .and_then(|mut w| writeln!(&mut w, "{} {} {}", local_s, id, nv))
            .map_err(|e|{ error!("Error writing log file: {}", e)});

        self.n_updates += 1;
        self.last_ts = nv.ts;
        self.values.insert(id, nv);

        //write status file
        let _ = &(self.values).iter().fold(
            File::create(&self.status).map(|f| BufWriter::new(f)),
            |r, (k, v)|
                r.and_then(|mut w| writeln!(&mut w, "{} -> {}", k, v).map(|_| w))
        ).map_err(|e|{ error!("Error writing status file: {}", e)});

    }

    pub fn run(&mut self, recv: Receiver<UWEvt>) {
        while let Ok(msg) = recv.recv() {
            debug!("SRV_MSG: {}", msg);

            match msg {
                UWEvt::NetUpdate(id, nv) => self.handle_update(id, nv),
                UWEvt::Heartbeat => ()
            };

            //Write heartbeat
            let _ = File::create(&self.heartbeat)
                .and_then(|mut w| writeln!(&mut w, "{} {} {} {}",
                                           self.n_logged_current, self.n_logged_total,
                                           self.n_updates, self.last_ts))
                .map_err(|e|{ error!("Error writing heartbeat file: {}", e)});
        }
    }
}