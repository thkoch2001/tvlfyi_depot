{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "wlbr";
    repo = "cl-marshal";
    rev = "eff1b15f2b0af2f26f71ad6a4dd5c4beab9299ec";
    sha256 = "08qs6fhk38xpkkjkpcj92mxx0lgy4ygrbbzrmnivdx281syr0gwh";
  };

in depot.nix.buildLisp.library {
  name = "marshal";
  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "serialization-format.lisp"
    "coding-idiom.lisp"
    "marshal.lisp"
    "unmarshal.lisp"
  ];
}
