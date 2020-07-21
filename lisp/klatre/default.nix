{ depot, ... }:

depot.nix.buildLisp.library {
  name = "klatre";

  deps = with depot.third_party.lisp; [
    iterate
  ];

  srcs = [
    ./package.lisp
    ./klatre.lisp
  ];
}
