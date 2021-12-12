{ depot, pkgs, ... }:

let
  inherit (depot.nix.buildLisp) bundled;

  src = pkgs.fetchFromGitHub {
    owner = "rpav";
    repo = "CheckL";
    rev = "80328800d047fef9b6e32dfe6bdc98396aee3cc9";
    sha256 = "0bpisihx1gay44xmyr1dmhlwh00j0zzi04rp9fy35i95l2r4xdlx";
  };

in depot.nix.buildLisp.library {
  name = "checkl";
  deps = with depot.third_party.lisp; [ (bundled "asdf") marshal fiveam ];

  srcs =
    map (f: src + ("/" + f)) [ "package.lisp" "checkl.lisp" "formalize.lisp" ];
}
