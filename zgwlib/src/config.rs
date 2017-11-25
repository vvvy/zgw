use std::path::{Path, PathBuf};
use std::fs::{OpenOptions, File};
use log::LogLevelFilter;
use simplelog::*;
use Result;
use serde;
use serde_yaml;
use std;

#[derive(Serialize, Deserialize)]
pub struct GenOptions {
    pub log_file: Option<PathBuf>,
    pub log_level: String,
    pub daemonize: bool
}

#[derive(Serialize, Deserialize)]
pub struct DaemonizeOptions {
    pub pid_file: Option<String>,
    pub chown_pid_file: Option<bool>,
    pub user: Option<String>,
    pub group: Option<String>,
    pub group_n: Option<u32>,
    pub umask: Option<u32>
}


pub fn read_config<T>(file: &str) -> Result<T> where T: serde::de::DeserializeOwned {
    File::open(file).map_err(
        |e| format!("read_config: I/O error: {}", e)
    ).and_then(
        |f| serde_yaml::from_reader(f).map_err(
            |e| format!("read_config: yaml error: {}", e)
        )
    )
}

pub fn read_default_config<T>(file: &str) -> Result<T> where
        for<'de> T: serde::Deserialize<'de> + std::default::Default {
    use std::io::ErrorKind;
    match File::open(file) {
        Ok(f) => serde_yaml::from_reader(f).map_err(
            |e| format!("read_config: yaml error: {}", e)
        ),
        Err(ref e) if e.kind() == ErrorKind::NotFound =>
            Ok(T::default()),
        Err(e) =>
            Err(format!("Could not read config file '{}': {}", file, e))
    }
}

pub fn save_config<T>(file: &str, cfg: &T) -> Result<()> where T: serde::Serialize {
    File::create(file).map_err(
        |e| format!("save_config: I/O error: {}", e)
    ).and_then(
        |mut f| serde_yaml::to_writer(&mut f, cfg).map_err(
            |e| format!("save_config: yaml error: {}", e)
        )
    )
}


pub fn save_config_file<T>(file: &str, cfg: &T) -> ! where T: serde::Serialize {
    std::process::exit(
        match save_config(file, cfg) {
            Err(e) => {
                println!("Error saving config to '{}': {}", file, e);
                1
            },
            Ok(_) => {
                println!("Config saved to '{}'", file);
                0
            }
        }
    );
}

/// '--sw=arg' => '--sw' 'arg'
/// '-abc' => -a -b -c
pub fn convert_arg(v: String) -> Vec<String> {
    use std::iter::FromIterator;
    if v.starts_with("--") {
        v.splitn(2, "=").map(|r| r.to_string()).collect()
    } else if v.starts_with("-") && v != "-" {
        v.chars().skip(1).map(|c| String::from_iter(vec!['-', c])).collect()
    } else {
        vec![v]
    }
}

pub fn bool_opt(s: String) -> bool {
    match s.as_ref() {
        "true"|"+"|"yes" => true,
        "false"|"-"|"no" => false,
        v => panic!("invalid bool value '{}'", v)
    }
}

fn log_level(s: &str) -> LogLevelFilter {
    match s {
        "trace" => LogLevelFilter::Trace,
        "debug" => LogLevelFilter::Debug,
        "info" => LogLevelFilter::Info,
        "warn" => LogLevelFilter::Warn,
        "error" => LogLevelFilter::Error,
        "off" => LogLevelFilter::Off,
        v => panic!("invalid loglevel value '{}'", v)
    }
}

/// Initializes logging and goes daemon. Returns process' CWD.
/// default_logfile should be "zgwa.log" or "zgws.log"
pub fn pre_start(mut g: GenOptions, d: DaemonizeOptions, default_logfile: &str) -> PathBuf {
    let cwd = std::env::current_dir().expect("cannot get CWD");

    if g.daemonize && daemonize(d, &cwd.to_owned()) && g.log_file.is_none() {
        g.log_file = Some(cwd.join(Path::new(default_logfile)));
    }

    let _ = match g.log_file {
        Some(file) =>
            WriteLogger::init(
                log_level(&g.log_level),
                Config::default(),
                OpenOptions::new().create(true).append(true).open(&file).expect(
                    &format!("Unable to open log file '{}'", file.as_path().to_string_lossy())
                )
            ),
        None => SimpleLogger::init(log_level(&g.log_level), Config::default())
    }.expect("Error initializing logging subsystem");

    info!("starting up");

    cwd
}

//--------------------------------------------------------------------------------------------

#[cfg(unix)]
extern crate daemonize;

#[cfg(unix)]
fn daemonize(opts: DaemonizeOptions, cwd: &Path) -> bool {
    let mut d = daemonize::Daemonize::new();

    d = d.working_directory(cwd);

    match opts.pid_file {
        Some(v) => d = d.pid_file(v),
        None => ()
    }
    match opts.chown_pid_file {
        Some(v) => d = d.chown_pid_file(v),
        None => ()
    }
    match opts.user {
        Some(v) => d = d.user(&v as &str),
        None => ()
    }
    match opts.group {
        Some(v) => d = d.group(&v as &str),
        None => ()
    }
    match opts.group_n {
        Some(v) => d = d.group(v),
        None => ()
    }
    match opts.umask {
        Some(v) => d = d.umask(v),
        None => ()
    }
    /*
        .pid_file("/tmp/test.pid") // Every method except `new` and `start`
        .chown_pid_file(true)      // is optional, see `Daemonize` documentation
        .working_directory("/tmp") // for default behaviour.
        .user("nobody")
        .group("daemon") // Group name
        //.group(2)        // or group id.
        .umask(0o777)    // Set umask, `0o027` by default.
        .privileged_action(|| "Executed before drop privileges");
        */

    match d.start() {
        Ok(_) => true,
        Err(e) => { println!("daemonize error: {}", e); false }
    }
}

#[cfg(windows)]
pub fn daemonize(_opts: DaemonizeOptions, _cwd: &Path) -> bool {
    false
}