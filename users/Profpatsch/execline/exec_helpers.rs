use std::os::unix::process::CommandExt;
use std::ffi::OsStr;
use std::os::unix::ffi::{OsStringExt, OsStrExt};

pub fn args_for_exec(current_prog_name: &str, no_of_positional_args: usize) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
    let mut args = std::env::args_os();
    // remove argv[0]
    let _ = args.nth(0);
    let mut args = args.map(|arg| arg.into_vec());
    let mut pos_args = vec![];
    // get positional args
    for i in 1..no_of_positional_args {
            pos_args.push(
                args.nth(0).expect(
                    &format!("{}: expects {} positional args, only got {}", current_prog_name, no_of_positional_args, i))
            );
    }
    // prog... is the rest of the iterator
    let prog : Vec<Vec<u8>> = args.collect();
    (pos_args, prog)
}

pub fn exec_into_args<'a, 'b, Args, Arg, Env, Key, Val>(current_prog_name: &str, args: Args, env_additions: Env) -> !
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
    let prog = args.nth(1).expect(&format!("{}: first argument must be an executable", current_prog_name));
    // TODO: same here
    let env = env_additions.into_iter().collect::<Vec<(Key, Val)>>();
    let env = env.iter().map(|(k,v)| (OsStr::from_bytes(k.as_ref()), OsStr::from_bytes(v.as_ref())));
    let err = std::process::Command::new(prog).args(args).envs(env).exec();
    panic!("{}: exec failed: {:?}", current_prog_name, err);
}

