{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "archimag";
    repo = "cl-routes";
    rev = "1b79e85aa653e1ec87e21ca745abe51547866fa9";
    sha256 = "1zpk3cp2v8hm50ppjl10yxr437vv4552r8hylvizglzrq2ibsbr1";
  };

in depot.nix.buildLisp.library {
  name = "routes";

  deps = with depot.third_party.lisp; [
    puri
    iterate
    split-sequence
  ];

  srcs = map (f: src + ("/src/" + f)) [
    "package.lisp"
    "uri-template.lisp"
    "route.lisp"
    "mapper.lisp"
  ];

  badImplementations = [
    "ccl" # TODO(sterni): undeclared ignored variables
  ];
}
