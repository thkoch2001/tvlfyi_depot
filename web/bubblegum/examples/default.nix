{ depot, pkgs, lib, ... }:

let

  scripts = [
    ./hello.nix
    ./derivation-svg.nix
    (substituteAll {
      src = ./blog.nix;
      # by making this a plain string this
      # can be something outside the nix store!
      blogdir = ./posts;
    })
  ];

  inherit (depot.nix) writeExecline runExecline getBins;

  inherit (depot.web.bubblegum) writeCGI;

  inherit (pkgs) runCommandLocal substituteAll;

  bins = (getBins pkgs.thttpd [ "thttpd" ])
    // (getBins pkgs.coreutils [ "printf" "cp" "mkdir" ]);

  webRoot = let
    copyScripts = lib.concatMap (path:
      let
        cgi = writeCGI {
          # assume we are on NixOS since thttpd doesn't set PATH.
          # using third_party.nix is tricky because not everyone
          # has a tvix daemon running.
          binPath = "/run/current-system/sw/bin";
        } path;
      in [ "if" [ bins.cp cgi "\${out}/${cgi.name}" ] ]) scripts;
  in runExecline.local "webroot" { }
  ([ "importas" "out" "out" "if" [ bins.mkdir "-p" "$out" ] ] ++ copyScripts);

  port = 9000;

in writeExecline "serve-examples" { } [
  "foreground"
  [
    bins.printf
    ''
      %s
    ''
    "Running on http://localhost:${toString port}"
  ]
  "${bins.thttpd}"
  "-D"
  "-p"
  (toString port)
  "-l"
  "/dev/stderr"
  "-c"
  "*.nix"
  "-d"
  webRoot
]
