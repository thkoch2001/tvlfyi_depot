# Closer to MOP is a compatibility layer that rectifies many of the
# absent or incorrect CLOS MOP features across a broad range of Common
# Lisp implementations
{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "pcostanza";
  repo = "closer-mop";
  rev = "8ec9577029b08ade5978236121c9ac276f78d8be"; # 2021-07-30
  sha256 = "0dm8xsa3hzpxjd7x248pbzd8blw01a8ls7spalzgbg1g7vbn6zg5";
};
in depot.nix.buildLisp.library {
  name = "closer-mop";

  srcs = [
    "${src}/closer-mop-packages.lisp"
    "${src}/closer-mop-shared.lisp"
    {
      sbcl = "${src}/closer-sbcl.lisp";
      ecl = "${src}/closer-ecl.lisp";
      ccl = "${src}/closer-clozure.lisp";
    }
  ];
}
