extern crate serde_json;

use serde_json::Value;
use std::ffi::OsString;
use std::os::unix::ffi::{OsStringExt, OsStrExt};
use std::io::{Error, ErrorKind, Write, stdout, stderr};
use std::process::Command;

fn render_nix_string(s: &OsString) -> OsString {
    let mut rendered = Vec::new();

    rendered.extend(b"\"");

    for b in s.as_os_str().as_bytes() {
        match char::from(*b) {
            '\"' => rendered.extend(b"\\\""),
            '\\' => rendered.extend(b"\\\\"),
            '$'  => rendered.extend(b"\\$"),
            _    => rendered.push(*b),
        }
    }

    rendered.extend(b"\"");

    OsString::from_vec(rendered)
}

fn render_nix_list(arr: &[OsString]) -> OsString {
    let mut rendered = Vec::new();

    rendered.extend(b"[ ");

    for el in arr {
        rendered.extend(render_nix_string(el).as_os_str().as_bytes());
        rendered.extend(b" ");
    }

    rendered.extend(b"]");

    OsString::from_vec(rendered)
}

fn main() -> std::io::Result<()> {
    let mut nix_args = Vec::new();

    let mut args = std::env::args_os().into_iter();
    let mut in_args = true;

    let mut argv: Vec<OsString> = Vec::new();

    // skip argv[0]
    args.next();

    loop {
        let arg = match args.next() {
            Some(a) => a,
            None => break,
        };

        if !arg.to_str().map(|s| s.starts_with("-")).unwrap_or(false) {
            in_args = false;
        }

        if in_args {
            match(arg.to_str()) {
                Some("--arg") | Some("--argstr") => {
                    nix_args.push(arg);
                    nix_args.push(args.next().unwrap());
                    nix_args.push(args.next().unwrap());
                    Ok(())
                }
                _ => Err(Error::new(ErrorKind::Other, "unknown argument")),
            }?
        } else {
            argv.push(arg);
        }
    }

    if argv.len() < 1 {
        Err(Error::new(ErrorKind::Other, "missing argv"))
    } else {
        let cd = std::env::current_dir()?.into_os_string();

        nix_args.push(OsString::from("--arg"));
        nix_args.push(OsString::from("currentDir"));
        nix_args.push(cd);

        nix_args.push(OsString::from("--arg"));
        nix_args.push(OsString::from("argv"));
        nix_args.push(render_nix_list(&argv[..]));

        nix_args.push(OsString::from("--eval"));
        nix_args.push(OsString::from("--json"));

        nix_args.push(argv[0].clone());

        let run = Command::new("nix-instantiate")
                          .args(nix_args)
                          .output()?;

        match serde_json::from_slice(&run.stdout[..]) {
            Ok(Value::String(s)) => stdout().write_all(s.as_bytes()),
            Ok(_) => Err(Error::new(ErrorKind::Other, "output must be a string")),
            _ => {
                stderr().write_all(&run.stderr[..]);
                Err(Error::new(ErrorKind::Other, "internal nix error"))
            },
        }
    }
}
