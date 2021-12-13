{ depot, briefcase, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/fukamachi/prove.git";
    rev = "5d71f02795b89e36f34e8c7d50e69b67ec6ca2de";
  };
in depot.nix.buildLisp.library {
  name = "prove";
  deps = [
    depot.third_party.lisp.cl-ppcre
    depot.third_party.lisp.cl-ansi-text
    depot.third_party.lisp.alexandria
    depot.third_party.lisp.uiop
    briefcase.third_party.lisp.cl-colors
  ];
  srcs = [
    "${src}/src/asdf.lisp"
    "${src}/src/suite.lisp"
    "${src}/src/color.lisp"
    "${src}/src/output.lisp"
    "${src}/src/prove.lisp"
    "${src}/src/report.lisp"
    "${src}/src/reporter.lisp"
    "${src}/src/test.lisp"
    "${src}/src/reporter/dot.lisp"
    "${src}/src/reporter/fiveam.lisp"
    "${src}/src/reporter/list.lisp"
    "${src}/src/reporter/tap.lisp"
  ];
}
