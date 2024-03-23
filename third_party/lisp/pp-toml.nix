{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "pnathan";
    repo = "pp-toml";
    rev = "6297d65541b67e8e93c0f42941534ef82f25bf79";
    sha256 = "1aicbi39q6m32ygm34q9lj9bazhxkxw0p4ygpzrh74xgbdmm7wg4";
  };

in depot.nix.buildLisp.library {
  name = "pp-toml";

  deps = with depot.third_party.lisp; [
    alexandria
    cl-ppcre
    generic-comparability
    local-time
    parse-number
    split-sequence
    esrap
  ];

  srcs = [ (src + "/pp-toml.lisp") ];
}
