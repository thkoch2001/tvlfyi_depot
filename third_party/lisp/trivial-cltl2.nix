{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "Zulu-Inuoe";
    repo = "trivial-cltl2";
    rev = "8a3bda30dc25d2f65fcf514d0eb6e6db75252c61";
    sha256 = "08cnzb9rnczn4pn2zpf0587ny4wjy1mjndy885fz9pw7xrlx37ip";
  };

in depot.nix.buildLisp.library {
  name = "trivial-cltl2";
  srcs = map (f: src + ("/" + f)) [
    "cltl2.lisp"
  ];
}
