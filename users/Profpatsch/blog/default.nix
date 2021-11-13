{ depot, pkgs, lib, ... }:

let
  bins = depot.nix.getBins pkgs.lowdown [ "lowdown" ]
      // depot.nix.getBins pkgs.cdb [ "cdbget" "cdbmake" "cdbdump" ]
      // depot.nix.getBins pkgs.coreutils [ "mv" "cat" "printf" "test" ]
      // depot.nix.getBins pkgs.s6-networking [ "s6-tcpserver" ]
      // depot.nix.getBins pkgs.time [ "time" ]
      ;

  # /
  # TODO: use
  toplevel = [
    {
      route = [ "notes" ];
      name = "Notes";
      page = {cssFile}: router cssFile;
    }
    {
      route = [ "projects" ];
      name = "Projects";
      # page = projects;
    }
  ];

  # /notes/*
  notes = [
    {
      route = [ "notes" "preventing-oom" ];
      name = "Preventing out-of-memory (OOM) errors on Linux";
      page = {cssFile}: markdownToHtml {
        name = "preventing-oom";
        markdown = ./notes/preventing-oom.md;
        inherit cssFile;
      };
    }
    {
      route = [ "notes" "rust-string-conversions" ];
      name = "Converting between different String types in Rust";
      page = {cssFile}: markdownToHtml {
        name = "rust-string-conversions";
        markdown = ./notes/rust-string-conversions.md;
        inherit cssFile;
      };
    }
  ];

  projects = [
    {
      name = "lorri";
      description = "<code>nix-shell</code> replacement for projects";
      link = "https://github.com/nix-community/lorri";
    }
    {
      name = "netencode";
      description = ''A human-readble nested data exchange format inspired by <a href="https://en.wikipedia.org/wiki/Netstring">netstrings</a> and <a href="https://en.wikipedia.org/wiki/Bencode">bencode</a>.'';
      link = depotCgitLink { relativePath = "users/Profpatsch/netencode/README.md"; };
    }
    {
      name = "yarn2nix";
      description = ''nix dependency generator for the <a href="https://yarnpkg.com/"><code>yarn</code> Javascript package manager</a>'';
      link = "https://github.com/Profpatsch/yarn2nix";
    }
  ];

  posts = [
    {
      date = "2017-05-04";
      title = "Ligature Emulation in Emacs";
      subtitle = "It’s not pretty, but the results are";
      description = "How to set up ligatures using <code>prettify-symbols-mode</code> and the Hasklig/FiraCode fonts.";
      page = {cssFile}: markdownToHtml {
        name = "2017-05-04-ligature-emluation-in-emacs";
        markdown = ./posts/2017-05-04-ligature-emulation-in-emacs.md;
        inherit cssFile;
      };
      route = [ "posts" "2017-05-04-ligature-emluation-in-emacs" ];
      tags = ["emacs"];
    }
  ];

  # convert a markdown file to html via lowdown
  markdownToHtml = {
    name,
    # the file to convert
    markdown,
    # css file to add to the final result, as { route }
    cssFile
  }:
    depot.nix.runExecline "${name}.html" {} ([
      "importas" "out" "out"
      (depot.users.Profpatsch.lib.debugExec "")
      bins.lowdown
        "-s" "-Thtml"
      ] ++
        (lib.optional (cssFile != null) (["-M" "css=${mkRoute cssFile.route}"]))
      ++ [
        "-o" "$out"
        markdown
    ]);

  # takes a { route … } attrset and converts the route lists to an absolute path
  fullRoute = attrs: lib.pipe attrs [
    (map (x@{route, ...}: x // { route = mkRoute route; }))
  ];

  # a cdb from route to a netencoded version of data for each route
  router = cssFile: lib.pipe (notes ++ posts) [
    (map (r: with depot.users.Profpatsch.lens;
      lib.pipe r [
        (over (field "route") mkRoute)
        (over (field "page") (_ { inherit cssFile; }))
      ]))
    (map (x: {
      name = x.route;
      value = depot.users.Profpatsch.netencode.gen.dwim x;
    }))
    lib.listToAttrs
    (cdbMake "router")
  ];

  # Create a link to the given source file/directory, given the relative path in the depot repo.
  # Checks that the file exists at evaluation time.
  depotCgitLink = {
    # relative path from the depot root (without leading /).
    relativePath
  }:
    # TODO: we might want to provide this from toplevel;
    # it’s the path of depot in the current file system, as determined by nix.
    let depotPath = toString ../../..;
    in assert
         (lib.assertMsg
           (builtins.pathExists (depotPath + "/" + relativePath))
           "depotCgitLink: path /${relativePath} does not exist in depot");
      "https://code.tvl.fyi/tree/${relativePath}";

  # look up a route by path ($1)
  router-lookup = cssFile: depot.nix.writeExecline "router-lookup" { readNArgs = 1; } [
    cdbLookup (router cssFile) "$1"
  ];

  runExeclineStdout = name: args: cmd: depot.nix.runExecline name args ([
    "importas" "-ui" "out" "out"
    "redirfd" "-w" "1" "$out"
  ] ++ cmd);

  notes-index-html =
    let o = fullRoute notes;
    in ''
      <ul>
      ${scope o (o: ''
        <li><a href="${str o.route}">${esc o.name}</a></li>
      '')}
      </ul>
    '';

  notes-index = pkgs.writeText "notes-index.html" notes-index-html;

  # A simple mustache-inspired string interpolation combinator
  # that takes an object and a template (a function from o to string)
  # and returns a string.
  scope = o: tpl:
    if builtins.typeOf o == "list" then
      lib.concatMapStringsSep "\n" tpl o
    else if builtins.typeOf o == "set" then
      tpl o
    else throw "${lib.generators.toPretty {} o} not allowed in template";

  # string-escape html (TODO)
  str = s: s;
  # html-escape (TODO)
  esc = s: s;
  html = s: s;

  projects-index-html =
  let o = projects;
  in ''
    <dl>
    ${scope o (o: ''
      <dt><a href="${str o.link}">${esc o.name}</a></dt>
      <dd>${html o.description}</dd>
    '')}
    </dl>
  '';

  projects-index = pkgs.writeText "projects-index.html" projects-index-html;

  posts-index-html =
  let o = fullRoute posts;
  in ''
    <dl>
    ${scope o (o: ''
      <dt>${str o.date} <a href="${str o.route}">${esc o.title}</a></dt>
      <dd>${html o.description}</dd>
    '')}
    </dl>
  '';

  posts-index = pkgs.writeText "projects-index.html" posts-index-html;

  arglibNetencode = val: depot.nix.writeExecline "arglib-netencode" { } [
    "export" "ARGLIB_NETENCODE" (depot.users.Profpatsch.netencode.gen.dwim val)
    "$@"
  ];

  # A simple http server that serves the site. Yes, it’s horrible.
  site-server = { cssFile, port }: depot.nix.writeExecline "blog-server" {} [
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
      # TODO: factor this out of here, this is routing not serving
      "ifelse" [ bins.test "$path" "=" "/notes" ]
        [ "export" "content-type" "text/html"
          "export" "serve-file" notes-index
          depot.users.Profpatsch.netencode.env-splice-record
        ]
      "ifelse" [ bins.test "$path" "=" "/projects" ]
        [ "export" "content-type" "text/html"
          "export" "serve-file" projects-index
          depot.users.Profpatsch.netencode.env-splice-record
        ]
      "ifelse" [ bins.test "$path" "=" "/posts" ]
        [ "export" "content-type" "text/html"
          "export" "serve-file" posts-index
          depot.users.Profpatsch.netencode.env-splice-record
        ]
      # TODO: ignore potential query arguments. See 404 message
      "pipeline" [ (router-lookup cssFile) "$path" ]
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

  # run argv or $1 if argv returns a failure status code.
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

  # go from a list of path elements to an absolute route string
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


  # create a cdb record entry, as required by the cdbmake tool
  cdbRecord = key: val:
    "+${toString (builtins.stringLength key)},${toString (builtins.stringLength val)}:"
    + "${key}->${val}\n";

  # create a full cdbmake input from an attribute set of keys to values (strings)
  cdbRecords =
    with depot.nix.yants;
    defun [ (attrs (either drv string)) string ]
    (attrs:
      (lib.concatStrings (lib.mapAttrsToList cdbRecord attrs)) + "\n");

  # run cdbmake on a list of key/value pairs (strings
  cdbMake = name: attrs: depot.nix.runExecline "${name}.cdb" {
    stdin = cdbRecords attrs;
  } [
    "importas" "out" "out"
    depot.users.Profpatsch.lib.eprint-stdin
    "if" [ bins.cdbmake "db" "tmp" ]
    bins.mv "db" "$out"
  ];

  # look up a key ($2) in the given cdb ($1)
  cdbLookup = depot.nix.writeExecline "cdb-lookup" { readNArgs = 2; } [
    # cdb ($1) on stdin
    "redirfd" "-r" "0" "$1"
    # key ($2) lookup
    bins.cdbget "$2"
  ];

in depot.nix.utils.drvTargets {
   inherit
    router
    depotCgitLink
    site-server
    notes-index
    notes-index-html
    projects-index
    projects-index-html
    posts-index-html
    ;

}
