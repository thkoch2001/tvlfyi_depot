{ depot ? import <depot> {}, ... }:

depot.nix.buildLisp.library {
  name = "prelude";
  srcs = [
    ./prelude.lisp
  ];
}
