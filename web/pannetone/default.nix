{ depot, ... }:

depot.nix.buildLisp.program {
  name = "pannetone";

  deps = with depot.third_party.lisp; [
    cl-prevalence
    cl-who
    defclass-std
    hunchentoot
    easy-routes
    trivial-ldap

    depot.lisp.klatre
  ];

  srcs = [
    ./src/pannetone.lisp
  ];
}
