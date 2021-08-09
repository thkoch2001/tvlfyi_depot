{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "archimag";
    repo = "restas";
    rev = "81bbbab6b36f81f846f78e71232e9d3d15f6d952";
    sha256 = "00ng6jik1lwjw3bbxhijy8s0ml24lgm73liwrr01gcsb0r6wrjjn";
  };

in depot.nix.buildLisp.library {
  name = "restas";
  deps = with depot.third_party.lisp; [
    cffi
    hunchentoot
    bordeaux-threads
    routes
    alexandria
    data-sift
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "packages.lisp"
    "special.lisp"
    "declarations.lisp"
    "errors.lisp"
    "render.lisp"
    "context.lisp"
    "module.lisp"
    "route.lisp"
    "decorators.lisp"
    "vhost.lisp"
    "hunchentoot.lisp"
    "policy.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
