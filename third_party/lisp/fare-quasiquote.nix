{ depot, pkgs, ... }:

let

  src = fetchTarball {
    url = "http://beta.quicklisp.org/archive/fare-quasiquote/2019-05-21/fare-quasiquote-20190521-git.tgz";
    sha256 = "1g6q11l50kgija9f55lzqpcwvaq0ljiw8v1j265hnyg6nahjwjvg";
  };

in depot.nix.buildLisp.library {
  name = "fare-quasiquote";

  deps = with depot.third_party.lisp; [
    fare-utils
    named-readtables
    optima
  ];

  srcs = map (f: src + ("/" + f)) [
    "packages.lisp"
    "quasiquote.lisp"
    "pp-quasiquote.lisp"
    "fare-quasiquote-optima.lisp"
    "quasiquote-readtable.lisp"
  ];
}
