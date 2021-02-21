{ depot, pkgs, lib, ... }:

let

  scripts = [
    ./hello.nix
    (substituteAll {
      src = ./blog.nix;
      # by making this a plain string this
      # can be something outside the nix store!
      blogdir = ./posts;
    })
  ];

  inherit (depot.nix)
    writeExecline
    runExecline
    getBins
    ;

  inherit (pkgs)
    runCommandLocal
    substituteAll
    ;

  bins = (getBins pkgs.thttpd [ "thttpd" ])
      // (getBins depot.users.sterni.ncla-run [ "ncla-run" ])
      // (getBins pkgs.coreutils [
        "cat"
        "tee"
        "printf"
        "env"
        "cp"
        "chmod"
        "mkdir"
      ]);

  shebang = lib.concatStringsSep " " [
    "#!${bins.env}"
    "-S"
    "PATH=/run/current-system/sw/bin"
    "${bins.ncla-run}"
    # thttpd doesn't let global environment leak into here
    # if you have a TVIX™ daemon running:
    # "PATH=${lib.makeBinPath [ pkgs.nix ]}"
    # Hack: assume we are on NixOS:
    # use the most cursed /usr/bin/env feature, namely -S which
    # allows us to pass any number of arguments to our interpreter
    # instead of maximum one using plain shebang (it is actually
    # `#!<argv0> <argv1>\n` where argv1 may also contain spaces).
    "--arg depot '(import ${depot.depotPath} {})'"
  ];

  buildNixCgi = path:
    let
      name =
        if lib.isDerivation path
        then path.name
        else baseNameOf path;
    in
    runExecline.local name {} [
      "importas" "out" "out"
      "pipeline" [
        "foreground" [
          bins.printf "%s\n" shebang
        ]
        bins.cat path
      ]
      "if" [ bins.tee "$out" ]
      "exit" "0"
    ];

  webRoot =
    let
      copyScripts = lib.concatMap
        (path: let
          cgi = buildNixCgi path;
        in [
          "if" [ bins.cp cgi "\${out}/${cgi.name}" ]
          "if" [ bins.chmod "+x" "\${out}/${cgi.name}" ]
        ]) scripts;
    in runExecline.local "webroot" {} ([
      "importas" "out" "out"
      "if" [ bins.mkdir "-p" "$out" ]
    ] ++ copyScripts);

  port = 9000;

in
  writeExecline "serve-examples" {} [
    "foreground" [
      bins.printf "%s\n" "Running on http://localhost:${toString port}"
    ]
    "${bins.thttpd}" "-D" "-p" (toString port) "-l" "/dev/stderr"
                     "-c" "*.nix" "-d" webRoot
  ]
