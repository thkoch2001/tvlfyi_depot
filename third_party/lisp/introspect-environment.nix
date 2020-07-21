{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "Bike";
    repo = "introspect-environment";
    rev = "e3885d0c8d7efeefeefb6cb911587280ed584fc1";
    sha256 = "0gxmz6z7wb53xbwaqvnwax126y163jkb8sjn25cpnjfsazp0qwdg";
  };

in depot.nix.buildLisp.library {
  name = "introspect-environment";

  deps = with depot.third_party.lisp; [
    sb-cltl2
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "expander.lisp"
    "sbcl.lisp"
    "doc.lisp"
  ];
}
