{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "mmontone";
    repo = "easy-routes";
    rev = "dab613ff419a655036a00beecee026ab6e0ba430";
    sha256 = "06lnipwc6mmg0v5gybcnr7wn5xmn5xfd1gs19vbima777245bfka";
  };

in
depot.nix.buildLisp.library {
  name = "easy-routes";
  deps = with depot.third_party.lisp; [
    hunchentoot
    routes
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "util.lisp"
    "easy-routes.lisp"
    "routes-map-printer.lisp"
  ];

  brokenOn = [
    "ecl" # dynamic cffi
  ];
}
