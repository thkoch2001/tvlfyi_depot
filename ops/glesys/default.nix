{
  depot,
  lib,
  pkgs,
  ...
}:

depot.nix.readTree.drvTargets rec {
  # Provide a Terraform wrapper with the right provider installed.
  terraform = pkgs.terraform.withPlugins (_: [ depot.third_party.terraform-provider-glesys ]);

  validate = depot.tools.checks.validateTerraform {
    inherit terraform;
    name = "glesys";
    src = lib.cleanSource ./.;
    env.GLESYS_TOKEN = "ci-dummy";
  };
}
