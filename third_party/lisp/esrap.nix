{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "scymtym";
    repo = "esrap";
    rev = "c99c33a33ff58ca85e8ba73912eba45d458eaa72";
    sha256 = "0dcylqr93r959blz1scb5yd79qplqdsl3hbji0icq2yyxvam7cyi";
  };

in depot.nix.buildLisp.library {
  name = "esrap";

  deps = with depot.third_party.lisp; [
    alexandria
    trivial-with-current-source-form
  ];

  srcs = map (f: src + "/src/" + f) [
    # :module "early"
    "package.lisp"
    "types.lisp"
    "protocol.lisp"
    "variables.lisp"
    "conditions.lisp"
    "expressions.lisp"
    "rule.lisp"
    "results.lisp"

    # :module "cache"
    "cache/chunk.lisp"
    "cache/packrat.lisp"

    # :module "src"
    "macros.lisp"
    "context.lisp"
    "interface.lisp"
    "evaluator.lisp"
    "editor-support.lisp"
  ];
}
