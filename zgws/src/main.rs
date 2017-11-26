//#[macro_use]
extern crate zgwlib;

#[macro_use]
extern crate log;

#[macro_use]
extern crate serde_derive;

mod config;
mod w_server;
mod w_server_hyper;
mod report;

pub use zgwlib::*;
use zgwlib::config::*;
use config::*;


use std::path::{Path, PathBuf};
use std::sync::mpsc::channel;


fn main() {
    let mut cfg = read_default_config("zgws.yaml").expect("unable to process default config file");
    parse_command_line(&mut cfg);
    let AppConfig { g, d, u } = cfg;
    let cwd = pre_start(g, d, "zgws.log");
    run_server(u, cwd);
}

fn version() -> ! {
    println!(
            "{} ({}) version {}",
             env!("CARGO_PKG_DESCRIPTION"),
             env!("CARGO_PKG_NAME"),
             env!("CARGO_PKG_VERSION")
    );
    std::process::exit(0);
}

fn help() -> ! {
    println!(
        "Help

-h|--help                           this message
-v|--version                        print version information and exit

Configuration and CWD (these options are executed immediately once parsed):

-C|--config-file <filepath>         Read the config file specified
                                    Note: this overwrites all command line options up to this point
-W|--write-config-file <filepath>   Write configuration to the specified config file and exit
-x|--reset-config                   Reset cfg to default values. May be used to effectively
                                    ignore default config file
-w|--cd <path>                      chdir <path>

General:

-l|--log-file <filepath>            log file (default stdout if --fg or 'zwaygw.log' otherwise)
-L|--log-level off|error|warn|info|debug|trace
                                    Set log level

UW Server options:

-r|--server-hostport <ip:port>      Bind at specified ip:port (default 0.0.0.0:7899)
--r-n-logfiles <int>                Number of rotated log files to maintain (default 3),
                                    0 to truncate main logfile when full, <0 to disable rotation
--r-records-per-logfile <int>       Max records per logfile (default 10000)
--r-dir <dirpath>                   Directory to hold report files (default ./report)

UNIX only:

-f|--fg                         stay in foreground (do not daemonize)
--pid-file <filepath>           PID file path
--chown-pid-file <bool>         whether to chown pid file

");

    std::process::exit(0);
}

fn parse_command_line(cfg: &mut AppConfig) {
    match std::env::args().skip(1).flat_map(convert_arg).fold(None, |s, a| match s {
        None => match a.as_ref() {
            "-f"|"--fg" => { cfg.g.daemonize = false; None },
            "-x"|"--reset-config" => { *cfg = AppConfig::default(); None},
            "-h"|"--help" => help(),
            "-v"|"--version" => version(),
            _ => Some(a)
        },
        Some(ref a0) => {
            match a0.as_ref() {
                "-C"|"--config-file" => *cfg = read_config(&a).expect("unable to read config file"),
                "-W"|"--write-config-file" => save_config_file(&a, &cfg),
                "-w"|"--cd" => std::env::set_current_dir(PathBuf::from(a)).expect("unable to cd"),
                "-l"|"--log-file" => cfg.g.log_file = Some(PathBuf::from(a)),
                "-L"|"--log-level" => cfg.g.log_level = a,
                "-r"|"--server-hostport" => cfg.u.hostport = a,
                "--r-n-logfiles" => cfg.u.n_logs = a.parse().expect("expected an int to --r-n-logfiles"),
                "--r-records-per-logfile" => cfg.u.records_per_logfile = a.parse().expect("expected an int to --records-per-logfile"),
                "--r-dir" => cfg.u.report_dir = Some(PathBuf::from(a)),
                "--pid-file" => cfg.d.pid_file = Some(a),
                "--chown-pid-file" => cfg.d.chown_pid_file = Some(bool_opt(a)),
                "--daemon-user" => cfg.d.user = Some(a),
                "--daemon-group" => cfg.d.group = Some(a),
                "--daemon-group-n" => cfg.d.group_n = Some(a.parse().expect("expected unsigned int to --daemon-group-n")),
                _ => panic!("Invalid cmd line at `{} {}`", a0, a)
            };
            None
        }
    }) {
        None => (),
        Some(ref e) => panic!("Invalid cmd line at `{}`<EOL>", e)
    }

}


fn run_server(u: UOptions, cwd: PathBuf) {
    use w::*;
    use w_server::*;
    use w_server_hyper::*;
    use report::*;
    use std::fs::create_dir_all;

    let (send, recv) = channel::<UWEvt>();
    match run_uw_server_hyper(&u.hostport, UWServerRunner::new(send, 64), u.nv_types) {
        Ok(mut listener) => {
            //TODO: add these 3 to config + options
            let report_dir = u.report_dir.unwrap_or_else(||cwd.join(Path::new("report")));
            create_dir_all(report_dir.as_path()).expect("could not create report dir");
            let mut report = Reporter::new(report_dir, u.records_per_logfile, u.n_logs);
            report.run(recv);
            let _ = listener.close();
        },
        Err(e) =>
            error!("Error starting server: {}", e)
    }
}