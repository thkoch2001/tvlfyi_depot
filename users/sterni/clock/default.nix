{ depot, ... }:

let

  inherit (depot.nix)
    buildC
    ;

in
  buildC.program {
    name = "clock";
    buildConf = {
      static = true;
      commonFlags = [
        "-Os"
      ] ++ buildC.defaultBuildConf.commonFlags;
    };
    deps = [
      depot.users.sterni.su
    ];
    srcs = [
      ./main.c
    ];
    CFLAGS = [
      "-Werror"
      "-std=c99"
    ];
    meta.description = "dot time clock";
  }
