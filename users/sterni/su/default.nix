{ depot, lib, ... }:

let

  inherit (depot.nix)
    buildC
    ;

  inherit (depot.third_party)
    jq
    ;

  CFLAGS = [
    "-Wall"
    "-Wextra"
    "-Werror"
    "-Wno-type-limits"
    "-std=c99"
  ];

in lib.fix (su:
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
      ./url.c
    ];

    inherit CFLAGS;

    tests = [
      (buildC.test {
        name = "su";

        runDeps = [
          jq
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
  })
