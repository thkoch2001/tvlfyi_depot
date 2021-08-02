{ depot, pkgs, ... }:

let
  lib = depot.nix.buildLisp.library {
    name = "mblog";

    srcs = [
      ./package.lisp
      ./transformer.lisp
      ./mblog.lisp
    ];

    deps = [
      depot.third_party.lisp.alexandria
      depot.third_party.lisp.closure-html
      depot.third_party.lisp.cl-mime
    ];
  };

in

depot.nix.buildLisp.program {
  name = "mnote-html";

  srcs = [
    # FIXME(tazjin): this is ridiculous lol
    pkgs.emptyFile
  ];

  deps = [
    lib
  ];

  main = "mblog:mnote-html";
}
