{ depot, pkgs, ... }:

depot.nix.readTree.drvTargets rec {
  # Provide a Terraform wrapper with the right provider installed.
  terraform = pkgs.terraform.withPlugins (_: [
    depot.third_party.terraform-provider-glesys
  ]);

  validate = pkgs.runCommand "tf-validate-glesys" { } ''
    cp -r ${lib.cleanSource ./.} . && chmod -R u+w .
    ${terraform}/bin/terraform init -upgrade -backend=false -input=false
    ${terraform}/bin/terraform validate | tee $out
  '';
}
