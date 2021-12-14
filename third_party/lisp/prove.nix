{ depot, pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "https://github.com/fukamachi/prove.git";
    rev = "5d71f02795b89e36f34e8c7d50e69b67ec6ca2de";
    hash = "sha256:0ca6ha3zhmckq3ad9lxm6sbg4i0hg3m81xhan4dkxd3x9898jzpc";
  };
in depot.nix.buildLisp.library {
  name = "prove";

  deps = [
    depot.third_party.lisp.alexandria
    depot.third_party.lisp.cl-ansi-text
    depot.third_party.lisp.cl-colors
    depot.third_party.lisp.cl-ppcre
    (depot.nix.buildLisp.bundled "asdf")
  ];

  srcs = [
    "${src}/src/color.lisp"
    "${src}/src/output.lisp"
    "${src}/src/asdf.lisp"
    "${src}/src/report.lisp"
    "${src}/src/reporter.lisp"
    "${src}/src/reporter/fiveam.lisp"
    "${src}/src/reporter/list.lisp"
    "${src}/src/reporter/dot.lisp"
    "${src}/src/reporter/tap.lisp"
    "${src}/src/suite.lisp"
    "${src}/src/test.lisp"
    "${src}/src/prove.lisp"
  ];
}
