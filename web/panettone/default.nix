{ depot, ... }:

depot.nix.buildLisp.program {
  name = "panettone";

  deps = with depot.third_party.lisp; [
    cl-prevalence
    cl-who
    defclass-std
    easy-routes
    hunchentoot
    local-time
    trivial-ldap

    depot.lisp.klatre
  ];

  srcs = [
    ./src/panettone.lisp
  ];
}
