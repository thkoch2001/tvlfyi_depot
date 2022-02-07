use std::ffi::OsStr;
use std::os::unix::ffi::{OsStrExt, OsStringExt};
use std::os::unix::process::CommandExt;

pub fn no_args(current_prog_name: &str) -> () {
    let mut args = std::env::args_os();
    // remove argv[0]
    let _ = args.nth(0);
    if args.len() > 0 {
        die_user_error(
            current_prog_name,
            format!("Expected no arguments, got {:?}", args.collect::<Vec<_>>()),
        )
    }
}

pub fn args(current_prog_name: &str, no_of_positional_args: usize) -> Vec<Vec<u8>> {
    let mut args = std::env::args_os();
    // remove argv[0]
    let _ = args.nth(0);
    if args.len() != no_of_positional_args {
        die_user_error(
            current_prog_name,
            format!(
                "Expected {} arguments, got {}, namely {:?}",
                no_of_positional_args,
                args.len(),
                args.collect::<Vec<_>>()
            ),
        )
    }
    args.map(|arg| arg.into_vec()).collect()
}

pub fn args_for_exec(
    current_prog_name: &str,
    no_of_positional_args: usize,
) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
    let mut args = std::env::args_os();
    // remove argv[0]
    let _ = args.nth(0);
    let mut args = args.map(|arg| arg.into_vec());
    let mut pos_args = vec![];
    // get positional args
    for i in 1..no_of_positional_args + 1 {
        pos_args.push(args.nth(0).expect(&format!(
            "{}: expects {} positional args, only got {}",
            current_prog_name, no_of_positional_args, i
        )));
    }
    // prog... is the rest of the iterator
    let prog: Vec<Vec<u8>> = args.collect();
    (pos_args, prog)
}

pub fn exec_into_args<'a, 'b, Args, Arg, Env, Key, Val>(
    current_prog_name: &str,
    args: Args,
    env_additions: Env,
) -> !
where
    Args: IntoIterator<Item = Arg>,
    Arg: AsRef<[u8]>,
    Env: IntoIterator<Item = (Key, Val)>,
    Key: AsRef<[u8]>,
    Val: AsRef<[u8]>,
{
    // TODO: is this possible without collecting into a Vec first, just leaving it an IntoIterator?
    let args = args.into_iter().collect::<Vec<Arg>>();
    let mut args = args.iter().map(|v| OsStr::from_bytes(v.as_ref()));
    let prog = args.nth(0).expect(&format!(
        "{}: first argument must be an executable",
        current_prog_name
    ));
    // TODO: same here
    let env = env_additions.into_iter().collect::<Vec<(Key, Val)>>();
    let env = env
        .iter()
        .map(|(k, v)| (OsStr::from_bytes(k.as_ref()), OsStr::from_bytes(v.as_ref())));
    let err = std::process::Command::new(prog).args(args).envs(env).exec();
    die_missing_executable(
        current_prog_name,
        format!(
            "exec failed: {}, while trying to execing into {:?}",
            err, prog
        ),
    );
}

/// Exit 1 to signify a generic expected error
/// (e.g. something that sometimes just goes wrong, like a nix build).
pub fn die_expected_error<S>(current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    die_with(1, current_prog_name, msg)
}

/// Exit 100 to signify a user error (“the user is holding it wrong”).
/// This is a permanent error, if the program is executed the same way
/// it should crash with 100 again.
pub fn die_user_error<S>(current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    die_with(100, current_prog_name, msg)
}

/// Exit 101 to signify an unexpected crash (failing assertion or panic).
/// This is the same exit code that `panic!()` emits.
pub fn die_panic<S>(current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    die_with(101, current_prog_name, msg)
}

/// Exit 111 to signify a temporary error (such as resource exhaustion)
pub fn die_temporary<S>(current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    die_with(111, current_prog_name, msg)
}

/// Exit 126 to signify an environment problem
/// (the user has set up stuff incorrectly so the program cannot work)
pub fn die_environment_problem<S>(current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    die_with(126, current_prog_name, msg)
}

/// Exit 127 to signify a missing executable.
pub fn die_missing_executable<S>(current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    die_with(127, current_prog_name, msg)
}

fn die_with<S>(status: i32, current_prog_name: &str, msg: S) -> !
where
    S: AsRef<str>,
{
    eprintln!("{}: {}", current_prog_name, msg.as_ref());
    std::process::exit(status)
}
