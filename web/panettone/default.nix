{ depot, ... }:

depot.nix.buildLisp.program {
  name = "panettone";

  deps = with depot.third_party.lisp; [
    bordeaux-threads
    cl-json
    cl-ppcre
    cl-smtp
    cl-who
    defclass-std
    drakma
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
    ./src/email.lisp
    ./src/inline-markdown.lisp
    ./src/authentication.lisp
    ./src/model.lisp
    ./src/irc.lisp
    ./src/panettone.lisp
  ];

  tests = {
    deps = with depot.third_party.lisp; [
      fiveam
    ];

    srcs = [
      ./test/package.lisp
      ./test/model_test.lisp
      ./test/inline-markdown_test.lisp
    ];

    expression = "(fiveam:run!)";
  };

  brokenOn = [
    "ecl" # dependencies use dynamic cffi
    "ccl" # The value NIL is not of the expected type STRING. when loading model.lisp
  ];
}
