{ depot, briefcase, ... }:

depot.nix.buildLisp.library {
  name = "f";
  deps = with briefcase.lisp; [
    prelude
  ];
  srcs = [
    ./main.lisp
  ];
}
