{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "pnathan";
    repo = "generic-comparability";
    rev = "97364fb188d0df996fd909bd3d41f1cec0d692b3";
    sha256 = "01ma0cwirxarwwmdwflnh8kmysmr2smh5kyvzhb2074ljxg8yq2p";
  };

in
depot.nix.buildLisp.library {
  name = "generic-comparability";

  deps = with depot.third_party.lisp; [ alexandria ];

  srcs = [ (src + "/generic-comparability.lisp") ];
}
