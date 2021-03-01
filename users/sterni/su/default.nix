{ depot, lib, ... }:

let

  inherit (depot.nix)
    buildC
    ;

  CFLAGS = [
    "-Wall"
    "-Wextra"
    "-Werror"
    "-std=c99"
  ];

in lib.fix (su:
  buildC.library {
    name = "su";

    include = [
      ./include
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

        deps = [ su ];

        srcs = [
          ./tests/main.c
        ];

        inherit CFLAGS;
      };
    };
  })
