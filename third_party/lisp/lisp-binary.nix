# A library to easily read and write complex binary formats.
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "j3pic";
    repo = "lisp-binary";
    rev = "052df578900dea59bf951e0a6749281fa73432e4";
    sha256 = "1i1s5g01aimfq6lndcl1pnw7ly5hdh0wmjp2dj9cjjwbkz9lnwcf";
  };
in depot.nix.buildLisp.library {
  name = "lisp-binary";

  deps = with depot.third_party.lisp; [
    cffi
    quasiquote_2
    moptilities
    flexi-streams
    closer-mop
  ];

  srcs = map (f: src + ("/" + f)) [
    "utils.lisp"
    "integer.lisp"
    "float.lisp"
    "simple-bit-stream.lisp"
    "reverse-stream.lisp"
    "binary-1.lisp"
    "binary-2.lisp"
    "types.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
