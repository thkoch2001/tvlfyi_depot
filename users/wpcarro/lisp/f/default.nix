{ depot, ... }:

depot.nix.buildLisp.library {
  name = "f";
  deps = with depot.users.wpcarro.lisp; [
    prelude
  ];
  srcs = [
    ./main.lisp
  ];
}
