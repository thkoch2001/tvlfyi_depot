{ depot, ... }:

depot.nix.buildLisp.program {
  name = "pannetone";

  deps = with depot.third_party.lisp; [
    cl-prevalence
    cl-who
    hunchentoot
    uiop
    depot.lisp.klatre
  ];

  srcs = [
    ./src/pannetone.lisp
  ];
}
