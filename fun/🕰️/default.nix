{ depot, ... }:

let
  inherit (depot.nix)
    buildLisp
    ;

  lib = buildLisp.library {
    name = "lib🕰️";
    deps = [
      depot.third_party.lisp.local-time
    ];

    srcs = [
      ./lib.lisp
    ];
  };

  bin = buildLisp.program {
    name = "🕰️";
    deps = [
      depot.third_party.lisp.unix-opts
      depot.lisp.klatre
      {
        default = buildLisp.bundled "asdf";
        sbcl = buildLisp.bundled "uiop";
      }
      lib
    ];

    srcs = [
      ./bin.lisp
    ];

    main = "🕰️.bin:🚂";
  };
in bin // {
  inherit lib;
}
