{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "mrossini-ethz";
    repo = "parseq";
    rev = "5cd95b324b68255d89f27f8065f4c29674558b26";
    sha256 = "1f3vvxgyiv0xn2hzafhh63l3gnvn2vaxr5pi3ld7d340mka2ndg0";
  };

in depot.nix.buildLisp.library {
  name = "parseq";

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "conditions.lisp"
    "utils.lisp"
    "defrule.lisp"
  ];
}
