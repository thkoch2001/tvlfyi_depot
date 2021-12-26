{ depot, pkgs, ... }:

depot.nix.readTree.drvTargets {
  # Provide a Terraform wrapper with the right provider installed.
  terraform = pkgs.terraform.withPlugins(p: [
    p.keycloak
  ]);
}
