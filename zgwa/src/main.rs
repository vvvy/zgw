//#[macro_use]
extern crate zgwlib;

#[macro_use]
extern crate log;

#[macro_use]
extern crate serde_derive;

mod interim_tree;
mod json;
mod tree;
mod nvimpl;
mod z_client;
mod z_client_file;
mod z_client_hyper;
mod w_client;
mod w_client_hyper;
mod w_client_stdio;
mod config;

pub use zgwlib::*;
use zgwlib::config::*;
use config::*;

use std::path::PathBuf;
use std::sync::mpsc::channel;


fn main() {
    let mut cfg = read_default_config("zgwa.yaml").expect("unable to process default config file");
    parse_command_line(&mut cfg);
    let AppConfig { g, d, z, w, c } = cfg;
    let cwd = pre_start(g, d, "zgwa.log");
    run_client(z, w, c, cwd);
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

Z interface configuration:

-P|--z-files <dirpath>              feed jsons from this dir to zway input (test/debug)
-U|--z-url <urlbase>                use this zway HTTP base url
-S|--z-secure-url <urlbase>         use this zway HTTPS base url
-d|--z-delay <seconds>              zway polling initial delay (default 0)
-e|--z-period <seconds>             maximum/default zway polling period (default 60)
-i|--z-period-min <seconds>         minimum zway polling period (default 5)

W interface configuration:

-A|--w-files <ignored>              log all W interface requests
-Q|--w-url <url>                    use this HTTP URL
-X|--w-secure-url <url>             use this HTTPS URL
-j|--w-max-send-count <int>         max number of messages per outgoing request (default 64)
-B|--w-period-long <seconds>        default period between W interface invocations (default 60)
-b|--w-period-short <seconds>       short period between W interface invocations. Used when there
                                    are more messages to send from either side (default 5)
-y|--w-error-delay <seconds>        seconds to wait on an interface error such as internet
                                    connectivity (default 60)

UNIX only:

-f|--fg                         stay in foreground (do not daemonize)
--pid-file <filepath>           PID file path
--chown-pid-file <bool>         whether to chown pid file

");

    std::process::exit(0);
}

fn parse_command_line(cfg: &mut AppConfig) {
    let mut username: Option<String> = None;
    let mut password = "*".to_owned();

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
                "-P"|"--z-files" => cfg.z.sd = SD::Files(a),
                "-U"|"--z-url" => cfg.z.sd = SD::URL(a),
                "-S"|"--z-secure-url" => { cfg.z.sd = SD::SURL(a, Creds::new_opt(&username, &password)) },
                "-d"|"--z-delay" => cfg.z.delay_s = a.parse().expect("expected an int to `--z-delay`"),
                "-e"|"--z-period" => cfg.z.period_max_s = a.parse().expect("expected an int to `--z-period`"),
                "-i"|"--z-period-min" => cfg.z.period_min_s = a.parse().expect("expected an int to `--z-period-min`"),
                "-A"|"--w-files" => cfg.w.sd = SD::Files(a),
                "-Q"|"--w-url" => cfg.w.sd = SD::URL(a),
                "-X"|"--w-secure-url" => { cfg.w.sd = SD::SURL(a, Creds::new_opt(&username, &password)) },
                "-j"|"--w-max-send-count" => cfg.w.max_send_count = a.parse().expect("expected unsigned int to --w-max-send-count"),
                "-B"|"--w-period-long"  => cfg.w.period_long_s = a.parse().expect("expected unsigned int to --w-period-long"),
                "-b"|"--w-period-short"  => cfg.w.period_short_s = a.parse().expect("expected unsigned int to --w-period-short"),
                "-y"|"--w-error-delay"  => cfg.w.delay_error_s = a.parse().expect("expected unsigned int to --w-error-delay"),
                "-g"|"--device-config-line" => cfg.c.push(parse_line(&a).expect("error parsing --device-config-line")),
                "-u"|"--username" => username = match a.as_ref() { "-" => None, v => Some(v.to_owned()) },
                "-p"|"--password" => password = a,
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

fn run_client(mut z: ZOptions, mut w: WOptions, c: DeviceConfig, cwd: PathBuf) {
    use z_client::*;
    use z_client_file::*;
    use z_client_hyper::*;
    use w_client::*;
    use w_client_stdio::*;
    use w_client_hyper::*;
    use zgwlib::nv::WCmd;
    use std::time::Duration;

    let pt = PasswordTable::read(cwd).expect("cannot read password table");
    debug!("Password file: {} entries", pt.len());
    pt.replace(&mut z.sd);
    pt.replace(&mut w.sd);

    let (w_send, w_recv) = channel::<WCmd>();
    let (z_send, z_recv) = channel::<WCmd>();

    let z = run_z_client(
        ZConfig {
            delay: Duration::from_secs(z.delay_s),
            period_min: Duration::from_secs(z.period_min_s),
            period_max: Duration::from_secs(z.period_max_s)
        },
        z_recv,
        w_send,
        match z.sd {
            SD::Files(p) => Box::new(FileZClient::new(p)),
            SD::URL(u) => Box::new(HyperZClient::new(u)),
            SD::SURL(u, a) => Box::new(SecureHyperZClient::new(u, a.map(|c| c.into())))
        },
        c
    );

    let w = run_w_client(
        WConfig {
            max_send_count: w.max_send_count,
            period_long: Duration::from_secs(w.period_long_s),
            period_short: Duration::from_secs(w.period_short_s),
            delay_error: Duration::from_secs(w.delay_error_s)
        },
        w_recv,
        z_send,
        match w.sd {
            SD::Files(_) => Box::new(StdioWClient { }),
            SD::URL(u) => Box::new(HyperWClient::new(u)),
            SD::SURL(u, a) => Box::new(SecureHyperWClient::new(u, a.map(|c| c.into())))
        }
    );

    let (_, _) = (z.join(), w.join());
    //println!("{:?} {:?}", rz, rw);
}