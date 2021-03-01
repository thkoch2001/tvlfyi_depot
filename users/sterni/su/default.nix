{ depot, pkgs, lib, ... }:

let

  inherit (depot.nix)
    buildC
    ;

  CFLAGS = [
    "-Wall"
    "-Wextra"
    "-pedantic"
    "-Werror"
    "-Wno-type-limits"
    "-std=c11"
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
      ./xml.c
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
