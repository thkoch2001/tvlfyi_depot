{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "mrossini-ethz";
    repo = "physical-quantities";
    rev = "8feb66ef3293fcb9ff4c4bd3ee872bfc385a590e";
    sha256 = "1qznv0hmn2n7g9dxx1iw0qpr0pf2lnbahn0x0b3v50xzcb65kgig";
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

  badImplementations = [
    "ccl" # TODO(sterni): INLINE declaration for unknown function HAS-UNIT-P
  ];
}
