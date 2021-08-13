{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "Shinmera";
    repo = "LASS";
    rev = "f51b9e941ee0a2a1f76ba814dcef22f9fb5f69bf";
    sha256 = "11mxzyx34ynsfsrs8pgrarqi9s442vkpmh7kdpzvarhj7i97g8yx";
  };

in depot.nix.buildLisp.library {
  name = "lass";

  deps = with depot.third_party.lisp; [
    trivial-indent
    trivial-mimes
    physical-quantities
    parse-float
    cl-base64
    (depot.nix.buildLisp.bundled "asdf")
  ];

  srcs = map (f: src + ("/" + f)) [
    "package.lisp"
    "readable-list.lisp"
    "compiler.lisp"
    "property-funcs.lisp"
    "writer.lisp"
    "lass.lisp"
    "special.lisp"
    "units.lisp"
    "asdf.lisp"
  ];

  badImplementations = [
    "ccl" # physical-quantities is broken
  ];
}
