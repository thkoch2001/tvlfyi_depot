use std::ffi::OsString;
use std::io::Write;
use std::sync::Arc;

use clap::Parser;
use nix::mount::{mount, umount, MsFlags};
use nix::sched::{unshare, CloneFlags};
use nix::sys::signal::{kill, Signal};
use nix::sys::wait::{waitpid, WaitPidFlag, WaitStatus};
use nix::unistd::{self, fork, ForkResult};
use tracing::Level;
use tracing::{debug, error, info, trace, warn};
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::Layer;
use tvix_castore::fs::fuse::FuseDaemon;
use tvix_store::pathinfoservice::make_fs;

#[cfg(not(target_os = "macos"))]
fn default_threads() -> usize {
    std::thread::available_parallelism()
        .map(|threads| threads.into())
        .unwrap_or(4)
}
// On MacFUSE only a single channel will receive ENODEV when the file system is
// unmounted and so all the other channels will block forever.
// See https://github.com/osxfuse/osxfuse/issues/974
#[cfg(target_os = "macos")]
fn default_threads() -> usize {
    1
}

#[derive(Parser)]
struct Args {
    /// A global log level to use when printing logs.
    /// It's also possible to set `RUST_LOG` according to
    /// `tracing_subscriber::filter::EnvFilter`, which will always have
    /// priority.
    #[arg(long)]
    log_level: Option<Level>,

    /// Number of FUSE threads to spawn.
    #[arg(long, env, default_value_t = default_threads())]
    threads: usize,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    blob_service_addr: String,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    directory_service_addr: String,

    #[arg(long, env, default_value = "grpc+http://[::1]:8000")]
    path_info_service_addr: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // configure log settings
    let level = args.log_level.unwrap_or(Level::INFO);

    // Set up the tracing subscriber.
    let subscriber = tracing_subscriber::registry().with(
        tracing_subscriber::fmt::Layer::new()
            .with_writer(std::io::stderr)
            .compact()
            .with_filter(
                EnvFilter::builder()
                    .with_default_directive(level.into())
                    .from_env()
                    .expect("invalid RUST_LOG"),
            ),
    );
    subscriber.try_init()?;

    match unsafe { fork() } {
        Ok(ForkResult::Parent {
            child: child_pid, ..
        }) => {
            let mut exit_status = 1;
            loop {
                match waitpid(child_pid, Some(WaitPidFlag::WUNTRACED)) {
                    Ok(WaitStatus::Signaled(child, Signal::SIGSTOP, _)) => {
                        let _ = kill(unistd::getpid(), Signal::SIGSTOP);
                        let _ = kill(child, Signal::SIGCONT);
                    }
                    Ok(WaitStatus::Signaled(_, signal, _)) => {
                        kill(unistd::getpid(), signal).unwrap_or_else(|err| {
                            panic!("failed to send {} signal to our self: {}", signal, err)
                        });
                    }
                    Ok(WaitStatus::Exited(_, status)) => {
                        exit_status = status;
                        break;
                    }
                    Ok(what) => {
                        error!("unexpected wait event happend: {:?}", what);
                        break;
                    }
                    Err(e) => {
                        eprintln!("waitpid failed: {}", e);
                        break;
                    }
                };
            }
            info!("child_process exited, terminating…");
            std::process::exit(exit_status);
        }
        Ok(ForkResult::Child) => {
            run_child1(args);
        }
        Err(e) => {
            error!("fork failed: {}", e);
        }
    };

    Ok(())
}

fn run_child1(args: Args) -> ! {
    let uid = unistd::getuid();
    let gid = unistd::getgid();

    trace!("unshare");
    unshare(CloneFlags::CLONE_NEWNS | CloneFlags::CLONE_NEWUSER).expect("unshare failed");

    trace!("get cwd");
    let cwd = std::env::current_dir().expect("cannot get current working directory");
    trace!("set cwd");
    std::env::set_current_dir("/").expect("cannot change directory to /");

    // fixes issue #1 where writing to /proc/self/gid_map fails
    // see user_namespaces(7) for more documentation
    if let Ok(mut file) = std::fs::File::create("/proc/self/setgroups") {
        let _ = file.write_all(b"deny");
    }

    trace!("setup uidmap");
    let mut uid_map =
        std::fs::File::create("/proc/self/uid_map").expect("failed to open /proc/self/uid_map");
    uid_map
        .write_all(format!("{} {} 1", uid, uid).as_bytes())
        .expect("failed to write new uid mapping to /proc/self/uid_map");

    let mut gid_map =
        std::fs::File::create("/proc/self/gid_map").expect("failed to open /proc/self/gid_map");
    gid_map
        .write_all(format!("{} {} 1", gid, gid).as_bytes())
        .expect("failed to write new gid mapping to /proc/self/gid_map");

    // restore cwd
    std::env::set_current_dir(&cwd)
        .unwrap_or_else(|_| panic!("cannot restore working directory {}", cwd.display()));

    run_child2(args)
}
#[tokio::main()]
async fn run_child2(args: Args) -> ! {
    let (blob_service, directory_service, path_info_service, _nar_calculation_service) =
        tvix_store::utils::construct_services(
            args.blob_service_addr,
            args.directory_service_addr,
            args.path_info_service_addr,
        )
        .await
        .unwrap();

    let root = tempfile::tempdir().unwrap();
    debug!("root: {:?}", root.path());
    let rodir = tempfile::tempdir().unwrap();
    debug!("rodir: {:?}", rodir.path());

    let fs = make_fs(
        blob_service,
        directory_service,
        Arc::from(path_info_service),
        false,
        false,
    );

    info!(mount_path=?root.path(), "mounting");
    trace!("mount1");
    let mut fuse_daemon = FuseDaemon::new(fs, &root.path(), args.threads, false).unwrap();

    trace!("mount2");

    mount(
        Some("/nix/store"),
        rodir.path(),
        Some("none"),
        MsFlags::MS_BIND | MsFlags::MS_REC | MsFlags::MS_PRIVATE,
        None::<&[u8]>,
    )
    .unwrap();

    trace!("mount3");
    let data: OsString = format!(
        "lowerdir={}:{}",
        rodir.path().to_str().expect("can not convert from os str"),
        root.path().to_str().expect("can not convert from os str")
    )
    .into();
    mount(
        Some("overlay"),
        "/nix/store",
        Some("overlay"),
        MsFlags::empty(),
        Some(data.as_os_str()),
    )
    .unwrap();
    trace!("spawn");
    let err = std::process::Command::new("bash")
        .args(["--norc", "--noprofile"])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    info!("exited");

    umount("/nix/store").unwrap();
    umount(rodir.path()).unwrap();

    fuse_daemon.unmount().unwrap();
    debug!("unmounted");
    if !err.success() {
        warn!("failed to execute: {}", err);
    }
    drop(root);
    drop(rodir);
    std::process::exit(err.code().unwrap_or(1));
}
