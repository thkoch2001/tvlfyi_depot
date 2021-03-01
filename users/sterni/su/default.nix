{ depot, lib, ... }:

let

  inherit (depot.nix)
    buildC
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

    extra = {
      tests = buildC.program {
        name = "tests";

        deps = [
          su
        ];

        srcs = [
          ./tests/main.c
        ];

        inherit CFLAGS;
      };
    };
  })
