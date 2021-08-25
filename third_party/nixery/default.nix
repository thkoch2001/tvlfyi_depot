# Import the Nixery repository as-is, but pass our own package set
# instead of the pin it has.
{ depot, pkgs, ... }:

let
  inherit (depot.nix.utils) drvTargets;

  commit = "6c4a69fa4280f0154ce257a1dfd23fb463c1ec5b";
  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "nixery";
    rev = commit;
    sha256 = "0g1v7020vjiqa3k07cfm3jzg3gc6nl3hs3vyw4c9s5v0dmmhhsz4";
  };
in drvTargets (import src {
  inherit pkgs;
  commitHash = _: commit;
})
