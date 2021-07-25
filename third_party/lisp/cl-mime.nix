{ depot, pkgs, ... }:

let
  name = "cl-mime";
  src = pkgs.fetchFromGitHub {
    owner = "40ants";
    repo = name;
    rev = "d30a28e0a40393bd3af7d138daa05319ed2e9d07"; # 2020-12-06
    sha256 = "0qn8if0fj6vzc897pqqqs0m1y107gmzqngpqhqmwrcsp1ckj5k0v";
  };

  getSrcs = builtins.map (p: "${src}/${p}");
in

depot.nix.buildLisp.library {
  inherit name;

  srcs = getSrcs [
    "package.lisp"
    "cl-mime.asd"
    "utilities.lisp"
    "classes.lisp"
    "encoding.lisp"
    "headers.lisp"
    "parse-mime.lisp"
    "print-mime.lisp"
  ];

  deps = [
    (depot.nix.buildLisp.bundled "asdf")
    depot.third_party.lisp.cl-base64
    depot.third_party.lisp.cl-ppcre
    depot.third_party.lisp.cl-qprint
  ];
}
