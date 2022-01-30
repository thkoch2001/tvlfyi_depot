# Import the Nixery repository as-is, but pass our own package set
# instead of the pin it has.
{ depot, pkgs, ... }:

let
  inherit (depot.nix.readTree) drvTargets;

  commit = "601cd998077f77f257ad1a40fa488add8464650f";
  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "nixery";
    rev = commit;
    sha256 = "195rz25y3hfxcmniysajzjg7g69qhz7w06lql8fn0dbcdcxsq6g4";
  };
in
drvTargets (import src {
  inherit pkgs;
  commitHash = _: commit;
})
