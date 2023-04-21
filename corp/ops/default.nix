{ depot, lib, pkgs, ... }:

depot.nix.readTree.drvTargets rec {
  # Provide a Terraform wrapper with Yandex Cloud support.
  terraform = pkgs.terraform.withPlugins (p: [
    p.yandex
  ]);

  validate = depot.tools.checks.validateTerraform {
    inherit terraform;
    name = "corp";
    src = lib.cleanSource ./.;
  };

  deps = depot.tools.depot-deps.overrideDeps {
    tf-yandex = {
      attr = "corp.ops.terraform";
      cmd = "terraform";
    };
  };
}
