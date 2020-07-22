{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "paddymul";
    repo = "css-lite";
    rev = "6ee4e6212ed56943d665df163d2a834b122e6273";
    sha256 = "1lyvw7hcy09k5qs6icky23s13psqxncyc73hdyiimb66p0jx6k0d";
  };

in depot.nix.buildLisp.library {
  name = "css-lite";

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "css-lite.lisp"
    "utility.lisp"
    "lite-utility.lisp"
    "paren-css-lite.lisp"
  ];
}
