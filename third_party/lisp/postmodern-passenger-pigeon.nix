{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "fisxoj";
    repo = "postmodern-passenger-pigeon";
    rev = "4755188e99dfb64ef3c010c5830b7374446f6674";
    sha256 = "1q1lfbn2n5di5m1p2gxxy0dj64sja68hcwmzs9mn4srwy9l9fp90";
  };

in depot.nix.buildLisp.library {
  name = "postmodern-passenger-pigeon";

  deps = with depot.third_party.lisp; [ alexandria postmodern pp-toml quri ];

  srcs = map (f: src + ("/src/" + f)) [
    "configuration.lisp"
    "operations.lisp"
    "migration.lisp"
    "solver.lisp"
    "pigeon.lisp"
  ];
}
