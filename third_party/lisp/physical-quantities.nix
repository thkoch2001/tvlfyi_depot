{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "mrossini-ethz";
    repo = "physical-quantities";
    rev = "v0.2.1";
    sha256 = "0mb2s94s6fhw5vfa89naalw7ld11sdsszlqpz0c65dvpfyfmmdmh";
  };

in depot.nix.buildLisp.library {
  name = "physical-quantities";

  deps = with depot.third_party.lisp; [
    parseq
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "utils.lisp"
    "conditions.lisp"
    "unit-factor.lisp"
    "unit-database.lisp"
    "units.lisp"
    "quantity.lisp"
    "numeric.lisp"
    "parse-rules.lisp"
    "read-macro.lisp"
    "si-units.lisp"
  ];
}
