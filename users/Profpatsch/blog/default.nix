{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.lowdown [ "lowdown" ]
      // depot.nix.getBins pkgs.cdb [ "cdbget" "cdbmake" "cdbdump" ]
      // depot.nix.getBins pkgs.coreutils [ "mv" "cat" "printf" "tee" "env" "test" "echo" "printenv" ]
      // depot.nix.getBins pkgs.bash [ "bash" ]
      // depot.nix.getBins pkgs.s6-networking [ "s6-tcpserver" ]
      // depot.nix.getBins pkgs.time [ "time" ]
      ;

  me = depot.users.Profpatsch;

  renderNote = name: note: depot.nix.runExecline "${name}.html" {} [
    "importas" "out" "out"
    bins.lowdown "-s" "-Thtml" "-o" "$out" note
  ];

  notes = [
    {
      route = [ "notes" "preventing-oom" ];
      name = "Preventing OOM";
      page = renderNote "preventing-oom" ./notes/preventing-oom.md;
    }
    {
      route = [ "notes" "rust-string-conversions" ];
      name = "Converting between different String types in Rust";
      page = renderNote "rust-string-conversions" ./notes/rust-string-conversions.md;
    }
  ];

  router = lib.pipe notes [
    (map (x@{route, ...}: x // { route = mkRoute route; }))
    (map (x: {
      name = x.route;
      value = me.netencode.gen.dwim x;
    }))
    lib.listToAttrs
    (cdbMake "notes-router")
  ];

  router-lookup = depot.nix.writeExecline "router-lookup" { readNArgs = 1; } [
    cdbLookup router "$1"
  ];

  runExeclineStdout = name: args: cmd: depot.nix.runExecline name args ([
    "importas" "-ui" "out" "out"
    "redirfd" "-w" "1" "$out"
  ] ++ cmd);

  index = runExeclineStdout "index" {} [
    "backtick" "-in" "TEMPLATE_DATA" [ cdbDumpNetencode router ]
    "pipeline" [
      bins.printf ''
        <ul>
        {{#.}}
          <li><a href="{{key}}">{{val}}<a></li>
        {{/.}}
        </ul>
      ''
    ]
    me.netencode.netencode-mustache
  ];

  arglibNetencode = val: depot.nix.writeExecline "arglib-netencode" { } [
    "export" "ARGLIB_NETENCODE" (me.netencode.gen.dwim val)
    "$@"
  ];

  notes-server = { port }: depot.nix.writeExecline "blog-server" {} [
    (me.lib.runInEmptyEnv [ "PATH" ])
    bins.s6-tcpserver "127.0.0.1" port
    bins.time "--format=time: %es" "--"
    runOr return400
    "pipeline" [
      (arglibNetencode {
        what = "request";
      })
      me.read-http
    ]
    me.netencode.record-splice-env
    runOr return500
    "importas" "-i" "path" "path"
    "if" [ depot.tools.eprintf "GET \${path}\n" ]
    runOr return404
    "backtick" "-ni" "TEMPLATE_DATA" [
      "ifelse" [ bins.test "$path" "=" "/notes" ]
        [ "export" "content-type" "text/html"
          "export" "serve-file" index
          me.netencode.env-splice-record
        ]
      # TODO: ignore potential query arguments. See 404 message
      "pipeline" [ router-lookup "$path" ]
      me.netencode.record-splice-env
      "importas" "-ui" "page" "page"
      "export" "content-type" "text/html"
      "export" "serve-file" "$page"
      me.netencode.env-splice-record
    ]
    runOr return500
    "if" [
      "pipeline" [ bins.printf ''
        HTTP/1.1 200 OK
        Content-Type: {{{content-type}}}; charset=UTF-8
        Connection: close

      '' ]
      me.netencode.netencode-mustache
    ]
    "pipeline" [ "importas" "t" "TEMPLATE_DATA" bins.printf "%s" "$t" ]
    me.netencode.record-splice-env
    "importas" "-ui" "serve-file" "serve-file"
    bins.cat "$serve-file"
  ];

  runOr = depot.nix.writeExecline "run-or" { readNArgs = 1; } [
    "foreground" [ "$@" ]
    "importas" "?" "?"
    "ifelse" [ bins.test "$?" "-eq" "0" ]
    []
    "if" [ depot.tools.eprintf "runOr: exited \${?}, running \${1}\n" ]
    "$1"
  ];

  return400 = depot.nix.writeExecline "return400" {} [
    bins.printf "%s" ''
      HTTP/1.1 400 Bad Request
      Content-Type: text/plain; charset=UTF-8
      Connection: close

    ''
  ];

  return404 = depot.nix.writeExecline "return404" {} [
    bins.printf "%s" ''
      HTTP/1.1 404 Not Found
      Content-Type: text/plain; charset=UTF-8
      Connection: close

      This page doesn’t exist! Query arguments are not handled at the moment.
    ''
  ];

  return500 = depot.nix.writeExecline "return500" {} [
    bins.printf "%s" ''
      HTTP/1.1 500 Internal Server Error
      Content-Type: text/plain; charset=UTF-8
      Connection: close

      Encountered an internal server error. Please try again.
    ''
  ];

  split-stdin = depot.nix.writeExecline "split-stdin" { argMode = "env"; } [
    "pipeline" [ "runblock" "1" bins.bash "-c" ''${bins.tee} >("$@")'' "bash-split-stdin" ]
    "runblock" "-r" "1"
  ];

  capture-stdin = depot.nix.writers.rustSimple {
    name = "capture-stdin";
    dependencies = [ me.execline.exec-helpers ];
  } ''
    extern crate exec_helpers;
    use std::io::Read;
    fn main() {
      let (args, prog) = exec_helpers::args_for_exec("capture-stdin", 1);
      let valname = &args[1];
      let mut v : Vec<u8> = vec![];
      std::io::stdin().lock().read_to_end(&mut v).unwrap();
      exec_helpers::exec_into_args("capture-stdin", prog, vec![(valname, v)]);
    }
  '';

  on-stdin = depot.nix.writeExecline "on-stdin" { readNArgs = 1; } [
    "pipeline" [ bins.printf "%s" "$1" ]
    "$@"
  ];

  mkRoute = route: "/" + lib.concatMapStringsSep "/" urlencodeAscii route;

  # urlencodes, but only ASCII characters
  # https://en.wikipedia.org/wiki/Percent-encoding
  urlencodeAscii = urlPiece:
    let
      raw = [ "!" "#" "$" "%" "&" "'" "(" ")" "*" "+" "," "/" ":" ";" "=" "?" "@" "[" "]" ];
      enc = [ "%21" "%23" "%24" "%25" "%26" "%27" "%28" "%29" "%2A" "%2B" "%2C" "%2F" "%3A" "%3B" "%3D" "%3F" "%40" "%5B" "%5D" ];
      rest = [ "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-" "_" "." "~" ];
    in
    assert lib.assertMsg (lib.all (c: builtins.elem c (raw ++ rest)) (lib.stringToCharacters urlPiece))
      "urlencodeAscii: the urlPiece must only contain valid url ASCII characters, was: ${urlPiece}";
    builtins.replaceStrings raw enc urlPiece;


  cdbRecord = key: val:
    "+${toString (builtins.stringLength key)},${toString (builtins.stringLength val)}:"
    + "${key}->${val}\n";
  cdbRecords =
    with depot.nix.yants;
    defun [ (attrs (either drv string)) string ]
    (attrs:
      (lib.concatStrings (lib.mapAttrsToList cdbRecord attrs)) + "\n");

  cdbMake = name: attrs: depot.nix.runExecline "${name}.cdb" {
    stdin = cdbRecords attrs;
  } [
    "importas" "out" "out"
    me.lib.eprint-stdin
    "if" [ bins.cdbmake "db" "tmp" ]
    bins.mv "db" "$out"
  ];

  cdbLookup = depot.nix.writeExecline "cdb-lookup" { readNArgs = 2; } [
    # cdb ($1) on stdin
    "redirfd" "-r" "0" "$1"
    # key ($2) lookup
    bins.cdbget "$2"
  ];

  cdbDumpNetencode = depot.nix.writeExecline "cdb-dump-netencode" { readNArgs = 1; } [
    # cdb ($1) on stdin
    "pipeline" [
      "redirfd" "-r" "0" "$1"
      bins.cdbdump
    ]
    cdbListToNetencode
  ];

  cdbListToNetencode = depot.nix.writers.rustSimple {
    name = "cdb-list-to-netencode";
    dependencies = [
      depot.third_party.rust-crates.nom
      me.execline.exec-helpers
      me.netencode.netencode-rs
    ];
  } ''
    extern crate nom;
    extern crate exec_helpers;
    extern crate netencode;
    use std::collections::HashMap;
    use std::io::BufRead;
    use nom::{IResult};
    use nom::sequence::{tuple};
    use nom::bytes::complete::{tag, take};
    use nom::character::complete::{digit1, char};
    use nom::error::{context, ErrorKind, ParseError};
    use nom::combinator::{map_res};
    use netencode::{T, Tag};

    fn usize_t(s: &[u8]) -> IResult<&[u8], usize> {
        context(
            "usize",
            map_res(
                map_res(digit1, |n| std::str::from_utf8(n)),
                |s| s.parse::<usize>())
        )(s)
    }

    fn parse_cdb_record(s: &[u8]) -> IResult<&[u8], (&[u8], &[u8])> {
        let (s, (_, klen, _, vlen, _)) = tuple((
            char('+'),
            usize_t,
            char(','),
            usize_t,
            char(':')
        ))(s)?;
        let (s, (key, _, val)) = tuple((
            take(klen),
            tag("->"),
            take(vlen),
        ))(s)?;
        Ok((s, (key, val)))
    }

    fn main() {
        let mut res = vec![];
        let stdin = std::io::stdin();
        let mut lines = stdin.lock().split(b'\n');
        loop {
            match lines.next() {
                None => exec_helpers::die_user_error("cdb-list-to-netencode", "stdin ended but we didn’t receive the empty line to signify the end of the cdbdump input!"),
                Some(Err(err)) => exec_helpers::die_temporary("cdb-list-to-netencode", format!("could not read from stdin: {}", err)),
                Some(Ok(line)) =>
                    if &line == b"" {
                        // the cdbdump input ends after an empty line (double \n)
                        break;
                    } else {
                        match parse_cdb_record(&line) {
                            Ok((b"", (key, val))) => {
                                let (key, val) = match
                                    std::str::from_utf8(key)
                                    .and_then(|k| std::str::from_utf8(val).map(|v| (k, v))) {
                                    Ok((key, val)) => (key.to_owned(), val.to_owned()),
                                    Err(err) => exec_helpers::die_user_error("cdb-list-to-netencode", format!("cannot decode line {:?}, we only support utf8-encoded key/values pairs for now: {}", String::from_utf8_lossy(&line), err)),
                                };
                                let _ = res.push((key, val));
                            },
                            Ok((rest, _)) => exec_helpers::die_user_error("cdb-list-to-netencode", format!("could not decode record line {:?}, had some trailing bytes", String::from_utf8_lossy(&line))),
                            Err(err) => exec_helpers::die_user_error("cdb-list-to-netencode", format!("could not decode record line {:?}: {:?}", String::from_utf8_lossy(&line), err)),
                        }
                    }
            }
        }
        let list = T::List(res.into_iter().map(
            |(k, v)| T::Record(vec![(String::from("key"), T::Text(k)), (String::from("val"), T::Text(v))].into_iter().collect())
        ).collect());
        netencode::encode(&mut std::io::stdout(), &list.to_u());
    }

  '';


in depot.nix.utils.drvTargets {
   inherit
    router
    notes-server
    split-stdin
    cdbListToNetencode
    index
    router-lookup
    ;

}
