{ depot, ... }:

depot.nix.buildLisp.library {
  name = "klatre";

  deps = with depot.third_party.lisp; [ local-time ];

  srcs = [ ./package.lisp ./klatre.lisp ];
}
