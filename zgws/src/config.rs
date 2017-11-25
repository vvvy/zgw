use std::default::Default;
use std::path::PathBuf;
use zgwlib::config::*;

#[derive(Serialize, Deserialize)]
pub struct AppConfig {
    pub g: GenOptions,
    pub d: DaemonizeOptions,
    pub u: UOptions
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
            u: UOptions {
                hostport: "0.0.0.0:7899".to_string(),
                report_dir: None,
                records_per_logfile: 10000,
                n_logs: 3
            }
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct UOptions {
    pub hostport: String,
    pub report_dir: Option<PathBuf>,
    pub records_per_logfile: u32,
    pub n_logs: i8
}
/*
impl UOptions {
    pub fn default() -> UOptions {
        UOptions {
            hostport: "0.0.0.0:7899".to_string(),
            report_dir: None,
            records_per_logfile: 10000,
            n_logs: 3
        }
    }
}
*/