{ depot, pkgs, ... }:

depot.nix.readTree.drvTargets rec {
  # Provide a Terraform wrapper with the right provider installed.
  terraform = pkgs.terraform.withPlugins (p: [
    p.keycloak
  ]);

  validate = pkgs.runCommand "tf-validate-keycloak" { } ''
    cp -r ${lib.cleanSource ./.} . && chmod -R u+w .
    ${terraform}/bin/terraform init -upgrade -backend=false -input=false
    ${terraform}/bin/terraform validate | tee $out
  '';
}
