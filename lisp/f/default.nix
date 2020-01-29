{
  depot ? import <depot> {},
  universe ? import <universe> {},
  ...
}:

depot.nix.buildLisp.library {
  name = "f";
  deps = with universe.lisp; [
    prelude
  ];
  srcs = [
    ./main.lisp
  ];
}
