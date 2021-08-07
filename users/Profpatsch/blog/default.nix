{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.lowdown [ "lowdown" ]
      // depot.nix.getBins pkgs.cdb [ "cdbget" "cdbmake" "cdbdump" ]
      // depot.nix.getBins pkgs.coreutils [ "mv" "cat" "printf" "tee" "env" "test" "echo" "printenv" ]
      // depot.nix.getBins pkgs.bash [ "bash" ]
      // depot.nix.getBins pkgs.s6-networking [ "s6-tcpserver" ]
      // depot.nix.getBins pkgs.time [ "time" ]
      ;

  renderNote = name: note: depot.nix.runExecline "${name}.html" {} [
    "importas" "out" "out"
    bins.lowdown "-s" "-Thtml" "-o" "$out" note
  ];

  toplevel = [
    {
      route = [ "notes" ];
      name = "Notes";
      page = router;
    }
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

  notesFullRoute = lib.pipe notes [
    (map (x@{route, ...}: x // { route = mkRoute route; }))
  ];

  router = lib.pipe notesFullRoute [
    (map (x: {
      name = x.route;
      value = depot.users.Profpatsch.netencode.gen.dwim x;
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

  index = runExeclineStdout "index" {
    stdin = depot.users.Profpatsch.netencode.gen.dwim notesFullRoute;
  } [
    "withstdinas" "-in" "TEMPLATE_DATA"
    "pipeline" [
      bins.printf ''
        <ul>
        {{#.}}
          <li><a href="{{route}}">{{name}}<a></li>
        {{/.}}
        </ul>
      ''
    ]
    depot.users.Profpatsch.netencode.netencode-mustache
  ];

  arglibNetencode = val: depot.nix.writeExecline "arglib-netencode" { } [
    "export" "ARGLIB_NETENCODE" (depot.users.Profpatsch.netencode.gen.dwim val)
    "$@"
  ];

  notes-server = { port }: depot.nix.writeExecline "blog-server" {} [
    (depot.users.Profpatsch.lib.runInEmptyEnv [ "PATH" ])
    bins.s6-tcpserver "127.0.0.1" port
    bins.time "--format=time: %es" "--"
    runOr return400
    "pipeline" [
      (arglibNetencode {
        what = "request";
      })
      depot.users.Profpatsch.read-http
    ]
    depot.users.Profpatsch.netencode.record-splice-env
    runOr return500
    "importas" "-i" "path" "path"
    "if" [ depot.tools.eprintf "GET \${path}\n" ]
    runOr return404
    "backtick" "-ni" "TEMPLATE_DATA" [
      "ifelse" [ bins.test "$path" "=" "/notes" ]
        [ "export" "content-type" "text/html"
          "export" "serve-file" index
          depot.users.Profpatsch.netencode.env-splice-record
        ]
      # TODO: ignore potential query arguments. See 404 message
      "pipeline" [ router-lookup "$path" ]
      depot.users.Profpatsch.netencode.record-splice-env
      "importas" "-ui" "page" "page"
      "export" "content-type" "text/html"
      "export" "serve-file" "$page"
      depot.users.Profpatsch.netencode.env-splice-record
    ]
    runOr return500
    "if" [
      "pipeline" [ bins.printf ''
        HTTP/1.1 200 OK
        Content-Type: {{{content-type}}}; charset=UTF-8
        Connection: close

      '' ]
      depot.users.Profpatsch.netencode.netencode-mustache
    ]
    "pipeline" [ "importas" "t" "TEMPLATE_DATA" bins.printf "%s" "$t" ]
    depot.users.Profpatsch.netencode.record-splice-env
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

  capture-stdin = depot.nix.writers.rustSimple {
    name = "capture-stdin";
    dependencies = [ depot.users.Profpatsch.execline.exec-helpers ];
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
    depot.users.Profpatsch.lib.eprint-stdin
    "if" [ bins.cdbmake "db" "tmp" ]
    bins.mv "db" "$out"
  ];

  cdbLookup = depot.nix.writeExecline "cdb-lookup" { readNArgs = 2; } [
    # cdb ($1) on stdin
    "redirfd" "-r" "0" "$1"
    # key ($2) lookup
    bins.cdbget "$2"
  ];

in depot.nix.utils.drvTargets {
   inherit
    router
    notes-server
    index
    router-lookup
    ;

}
