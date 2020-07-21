

{ depot, pkgs, ... }:


let

  src = pkgs.fetchFromGitHub {
    owner = "guicho271828";
    repo = "lisp-namespace";
    rev = "28107cafe34e4c1c67490fde60c7f92dc610b2e0";
    sha256 = "1jw2wykp06z2afb9nm1lgfzll5cjlj36pnknjx614057zkkxq4iy";
  };

in depot.nix.buildLisp.library {
  name = "lisp-namespace";

  deps = with depot.third_party.lisp; [
    alexandria
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "namespace.lisp"
    "namespace-let.lisp"
  ];
}
