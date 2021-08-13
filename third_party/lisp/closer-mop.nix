# Closer to MOP is a compatibility layer that rectifies many of the
# absent or incorrect CLOS MOP features across a broad range of Common
# Lisp implementations
{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "pcostanza";
  repo = "closer-mop";
  rev = "e1d1430524086709a7ea8e0eede6849aa29d6276";
  sha256 = "1zda6927379pmrsxpg29jnj6azjpa2pms9h7n1iwhy6q9d3w06rf";
};
in depot.nix.buildLisp.library {
  name = "closer-mop";

  srcs = [
    "${src}/closer-mop-packages.lisp"
    "${src}/closer-mop-shared.lisp"
    {
      sbcl = "${src}/closer-sbcl.lisp";
      ecl = "${src}/closer-ecl.lisp";
      ccl = "${src}/closer-ccl.lisp";
    }
  ];
}
