use std::default::Default;
use std::path::{Path, PathBuf};
use std::fs::File;
use zgwlib::config::{GenOptions, DaemonizeOptions};
use nvimpl::*;
pub use nvimpl::DeviceConfig;
use nv::OperationTarget;
use Result;

#[derive(Serialize, Deserialize)]
pub struct AppConfig {
    pub g: GenOptions,
    pub d: DaemonizeOptions,
    pub z: ZOptions,
    pub w: WOptions,
    pub c: DeviceConfig
}

impl Default for AppConfig {
    fn default() -> AppConfig {
        AppConfig {
            g: GenOptions {
                log_file: None,
                log_level: "info".to_string(),
                daemonize: true
            },
            d: DaemonizeOptions {
                pid_file: None,
                chown_pid_file: None,
                user: None,
                group: None,
                group_n: None,
                umask: None
            },
            z: ZOptions {
                delay_s: 0,
                period_min_s: 5,
                period_max_s: 60,
                sd: SD::Files("../data".to_string())
            },
            w: WOptions {
                max_send_count: 64,
                period_long_s: 60,
                period_short_s: 5,
                delay_error_s: 120,
                sd: SD::Files("*".to_string())
            },
            c: Vec::new()
        }
    }
}



#[derive(Clone, Serialize, Deserialize)]
pub struct Creds { pub username: String, pub password: String }

impl Creds {
    pub fn new_opt(username: &Option<String>, password: &String) -> Option<Creds> {
        username.clone().map(|u| Creds { username: u, password: password.clone() })
    }
}

impl Into<(String, String)> for Creds {
    fn into(self) -> (String, String) { (self.username, self.password) }
}

#[derive(Clone, Serialize, Deserialize)]
pub enum SD {
    Files(String),
    URL(String),
    SURL(String, Option<Creds>)
}

#[derive(Serialize, Deserialize)]
pub struct ZOptions {
    pub delay_s: u64,
    pub period_min_s: u64,
    pub period_max_s: u64,
    pub sd: SD
}

#[derive(Serialize, Deserialize)]
pub struct WOptions {
    pub max_send_count: usize,
    pub period_long_s: u64,
    pub period_short_s: u64,
    pub delay_error_s: u64,
    pub sd: SD
}

//------------------------------------------------------------------------------------------------

fn parse_optarget(s: &str) -> Result<OperationTarget> {
    let air: Result<Vec<u8>> = s
        .split(',')
        .map(|s| s.trim())
        .map(|s| s.parse().map_err(|e| format!("Unable to convert `{}` to number: {}", s, e)))
        .collect()
    ;

    air
        .and_then(|a| match a.len() { 3 => Ok(a), n => Err(format!("OT: invalid #of args: {} (3 reqd)", n))})
        .map(|a| OperationTarget::new(a[0], a[1], a[2]))
}

fn parse_command<'s>(k: Vec<&'s str>, v: Vec<&'s str>) -> Result<ConfigCommand> {
    if k.is_empty() {
        return Err("Invalid command (empty key)".to_owned());
    }
    match (k[0], k.len() - 1, &k[1..], v.len()) {
        ("state", 1, sname, vl) if vl > 0 => {
            let state = match (v[0], vl - 1, &v[1..]) {
                ("monitor", 0, _) => Ok(ElementState::Monitor { }),
                ("updater", 2, tp_otr) => {
                    let tp: Result<UpdaterT> = match tp_otr[0] {
                        "readonly" => Ok(UpdaterT::Readonly),
                        "thermostat-setpoint-heating" => Ok(UpdaterT::ThermostatSetpointHeating),
                        "switch-binary" => Ok(UpdaterT::SwitchBinary),
                        v => Err(format!("Unknown updater-typespec `{}`", v))
                    };

                    tp.and_then(|t| parse_optarget(tp_otr[1]).map(|o|
                        ElementState::Updater{ t: t, ot: o }
                    ))
                },
                ("pusher", 1, nm) => Ok(ElementState::Pusher { target: nm[0].to_owned() }),
                _ => Err(format!("Invalid statespec {}<{}>", v[0], vl))

            };
            state.map(|s| ConfigCommand::State { name: sname[0].to_owned(), state: s })
        },
        ("link", 2, ll, _) =>
            Ok(ConfigCommand::Link{ from: ll[0].to_owned(), to: ll[1].to_owned() }),
        ("trigger", 1, t, 1) =>
            Ok(ConfigCommand::MountTrigger { trigger_mp: t[0].to_owned(), monitor: v[0].to_owned() }),
        ("wire-monitoring", 1, n, 2) =>
            parse_optarget(v[1]).map(|ot|
                ConfigCommand::WireMonitoring{ name: n[0].to_owned(), trigger_mp: v[0].to_owned(), ot: ot}
            ),
        _ =>
            Err("Commmand not recognized".to_owned())
    }
}

/// Config key/value field separator char
const FSEP: char = '/';

pub fn parse_line(line: &str) -> Result<ConfigCommand> {
    let mut fs = line.splitn(2, '=').map(|s| s.trim());
    let ko = fs.next().map(|f| f.split(FSEP).map(|s| s.trim()).collect());
    let vo = fs.next().map(|f| f.split(FSEP).map(|s| s.trim()).collect());
    match (ko, vo) {
        (Some(k), Some(v)) => parse_command(k, v)
            .map_err(|e| format!("Error: {} parsing command `{}`", e, line)),
        _ => Err(format!("Invalid config command: `{}`", line))
    }
}

//------------------------------------------------------------------------------------------------
// PasswordFile support
use std::collections::HashMap;

/// (url, user) -> password
pub struct PasswordTable {
    t: HashMap<(String, String), String>
}

impl PasswordTable {
    pub fn len(&self) -> usize {
        self.t.len()
    }
    pub fn replace(&self, input: &mut SD) {
        match input {
            &mut SD::SURL(ref url, Some(ref mut cred)) =>
                match self.t.get(&(url.clone(), cred.username.clone())) {
                    Some(newpwd) => cred.password = newpwd.clone(),
                    None => ()
                },
            _ =>
                ()
        }
    }

    pub fn read(dir: PathBuf) -> Result<PasswordTable> {
        use std::io::{ErrorKind, BufReader, BufRead};

        let fp = dir.join(Path::new(".zpasswd"));

        match File::open(&fp) {
            Ok(file) => {
                let mut r = HashMap::new();
                for rline in BufReader::new(file).lines() {
                    match rline {
                        Ok(line) => {
                            let mut fields = line.split_whitespace();
                            let ourl = fields.next();
                            let ouser = fields.next();
                            let opwd = fields.next();
                            if let (Some(url), Some(user), Some(pwd)) = (ourl, ouser, opwd) {
                                r.insert((url.to_owned(), user.to_owned()), pwd.to_owned());
                            } else {
                                return Err("invalid lines in password file".to_owned())
                            }
                        },
                        Err(e) => {
                            return Err(format!("Error reading password file: {}", e))
                        }
                    }
                }
                Ok(PasswordTable { t: r })
            },
            Err(e) =>
                if e.kind() == ErrorKind::NotFound {
                    Ok( PasswordTable { t: HashMap::new() })
                } else {
                    Err(format!("passwordfile at `{}` exists, but cannot be open: {}", fp.as_path().to_string_lossy(), e))
                }
        }
    }
}

//------------------------------------------------------------------------------------------------