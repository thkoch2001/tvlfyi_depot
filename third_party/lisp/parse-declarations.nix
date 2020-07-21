{ depot, pkgs, ... }:

let

  src = fetchTarball {
    url = "http://beta.quicklisp.org/archive/parse-declarations/2010-10-06/parse-declarations-20101006-darcs.tgz";
    sha256 = "04l3s180wxq6xyhgd77mbd03a1w1m0j9snag961g2f9dd77w6q1r";
  };

in depot.nix.buildLisp.library {
  name = "parse-declarations";
  srcs = map (f: src + ("/" + f)) [
    "parse-declarations.lisp"
  ];
}
