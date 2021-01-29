{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.lowdown [ "lowdown" ]
      // depot.nix.getBins pkgs.cdb [ "cdbget" "cdbmake" "cdbdump" ]
      // depot.nix.getBins pkgs.coreutils [ "mv" "cat" "printf" "tee" "env" ]
      // depot.nix.getBins pkgs.bash [ "bash" ]
      // depot.nix.getBins pkgs.s6-networking [ "s6-tcpserver" ]
      ;

  renderNote = name: note: depot.nix.runExecline "${name}.html" {} [
    "importas" "out" "out"
    bins.lowdown "-s" "-Thtml" "-o" "$out" note
  ];

  preventing-oom = renderNote "preventing-oom" ./notes/preventing-oom.md;

  notes = [
    {
      route = [ "notes" ];
      page = pkgs.writeText "notes.html" ''
        <ul>
          <li>Preventing OOM errors</li>
        </ul>
      '';
    }
    {
      route = [ "notes" "preventing-oom" ];
      page = preventing-oom;
    }
  ];

  router = lib.pipe notes [
    (map ({route, page}: {
      name = mkRoute route;
      value = page;
    }))
    lib.listToAttrs
    (cdbMake "notes-router")
  ];

  router-lookup = depot.nix.writeExecline "router-lookup" { readNArgs = 1; } [
    cdbLookup router "$1"
  ];

  arglibNetencode = val: depot.nix.writeExecline "arglib-netencode" { } [
    "export" "ARGLIB_NETENCODE" (depot.users.Profpatsch.netencode.gen.dwim val)
    "$@"
  ];

  blog-server = depot.nix.writeExecline "blog-server" {} [
    (depot.users.Profpatsch.lib.runInEmptyEnv [ "PATH" ])
    bins.s6-tcpserver "127.0.0.1" "8080"
    "pipeline" [
      (arglibNetencode {
        what = "request";
      })
      depot.users.Profpatsch.read-http
    ]
    "pipeline" [ depot.users.Profpatsch.netencode.record-get "headers" ]
    depot.users.Profpatsch.netencode.record-splice-env
    "if" [ bins.printf "%s" ''
      HTTP/1.1 200 OK
      Content-Type: text/plain; charset=UTF-8
      Connection: close

    ''
    ]
    bins.env

    #   method was: {{{method}}}
    #   path was: {{{path}}}
    #   headers:
    #   - user-agent: {{{headers.user-agent}}}
    #   - host: {{{headers.host}}}
    # ''
    # depot.users.Profpatsch.netencode.netencode-mustache
  ];

  split-stdin = depot.nix.writeExecline "split-stdin" { argMode = "env"; } [
    "pipeline" [ "runblock" "1" bins.bash "-c" ''${bins.tee} >("$@")'' "bash-split-stdin" ]
    "runblock" "-r" "1"
  ];

  capture-stdin = depot.users.Profpatsch.writers.rustSimple {
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

  mkRoute = route: lib.concatMapStringsSep "/" urlencodeAscii route;

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


in {
   inherit
    preventing-oom
    router
    blog-server
    split-stdin
    ;

}
