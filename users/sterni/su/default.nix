{ depot, pkgs, lib, ... }:

let

  inherit (depot.nix)
    buildC
    ;

  CFLAGS = [
    "-Werror"
    "-Wno-type-limits"
    "-std=c99"
  ];

in
  buildC.library {
    name = "su";
    include = [
      ./include/su
    ];

    srcs = [
      ./char.c
      ./check.c
      ./memstream.c
      ./json.c
      ./s.c
      ./time.c
      ./url.c
    ];

    inherit CFLAGS;

    tests = [
      (su: buildC.test {
        name = "su-check";

        runDeps = [
          pkgs.jq
        ];

        deps = [
          su
        ];

        srcs = [
          ./tests/main.c
        ];

        inherit CFLAGS;
      })
    ];
  }
