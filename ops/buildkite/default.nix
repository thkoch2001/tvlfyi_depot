{ depot, pkgs, ... }:

depot.nix.readTree.drvTargets {
  terraform = pkgs.terraform.withPlugins (p: [
    p.buildkite
  ]);
}
