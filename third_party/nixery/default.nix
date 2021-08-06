# Import the Nixery repository as-is, but pass our own package set
# instead of the pin it has.
{ depot, pkgs, ... }:

let
  inherit (depot.nix.utils) drvTargets;

  commit = "e85b7260b58e5cd60630c0f7e72d4eebe21c6617";
  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "nixery";
    rev = commit;
    sha256 = "1f459syg0ssw4l51yii83gq12gcsmdpzdq4kd2wj2qry6z16ayj4";
  };
in drvTargets (import src {
  inherit pkgs;
  commitHash = _: commit;
})
