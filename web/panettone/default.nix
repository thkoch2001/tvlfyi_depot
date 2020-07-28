{ depot, ... }:

depot.nix.buildLisp.program {
  name = "panettone";

  deps = with depot.third_party.lisp; [
    cl-prevalence
    cl-who
    drakma
    defclass-std
    easy-routes
    hunchentoot
    lass
    local-time
    postmodern
    trivial-ldap

    depot.lisp.klatre
  ];

  srcs = [
    ./panettone.asd
    ./src/packages.lisp
    ./src/util.lisp
    ./src/css.lisp
    ./src/model.lisp
    ./src/panettone.lisp
  ];
}
