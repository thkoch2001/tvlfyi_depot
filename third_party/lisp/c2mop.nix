{ depot, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "pcostanza";
    repo = "closer-mop";
    rev = "2a98c620d9b3a0a3a830fcd2ac31ea8337ba95a5";
    sha256 = "1hladm1s2yvkwqhfz4z5xyni1f31q401xcv65sr1d8sd7c1lbc7x";
  };

in depot.nix.buildLisp.library {
  name = "c2mop";

  deps = with depot.third_party.lisp; [
    alexandria
    lisp-namespace
    trivial-cltl2
  ];

  srcs = map (f: src + ("/" + f)) [
    "closer-mop-packages.lisp"
    "closer-mop-shared.lisp"
    "closer-sbcl.lisp"
  ];
}
