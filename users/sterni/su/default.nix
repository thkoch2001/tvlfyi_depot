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
    LDFLAGS = [
      "-lm"
    ];

    tests = [
      (su: buildC.test {
        name = "su-check";

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
  }
