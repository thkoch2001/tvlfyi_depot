{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "m2ym";
    repo = "optima";
    rev = "373b245b928c1a5cce91a6cb5bfe5dd77eb36195";
    sha256 = "1yw4ymq7ms89342kkvb3aqxgv0w38m9kd8ikdqxxzyybnkjhndal";
  };

in depot.nix.buildLisp.library {
  name = "optima";

  deps = with depot.third_party.lisp; [
    alexandria
    closer-mop
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "packages.lisp"
    "util.lisp"
    "runtime.lisp"
    "pattern.lisp"
    "fail.lisp"
    "compiler.lisp"
    "match.lisp"
    "extra.lisp"
  ];
}
