{ depot, ... }:

depot.nix.buildLisp.library {
  name = "klatre";

  srcs = [
    ./package.lisp
    ./klatre.lisp
  ];
}
