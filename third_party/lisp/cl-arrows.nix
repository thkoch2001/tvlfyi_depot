{ depot, ... }:

let
  src = builtins.fetchGit {
    url = "https://github.com/nightfly19/cl-arrows.git";
    rev = "cbb46b69a7de40f1161c9caaf6cef93b3af9994f";
  };
in depot.nix.buildLisp.library {
  name = "cl-arrows";
  deps = [];
  srcs = [
    "${src}/packages.lisp"
    "${src}/arrows.lisp"
  ];
}
