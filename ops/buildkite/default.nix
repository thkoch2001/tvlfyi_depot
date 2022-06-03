{ depot, lib, pkgs, ... }:

depot.nix.readTree.drvTargets rec {
  terraform = pkgs.terraform.withPlugins (p: [
    p.buildkite
  ]);

  validate = pkgs.runCommand "tf-validate-buildkite" { } ''
    cp -r ${lib.cleanSource ./.} . && chmod -R u+w .
    ${terraform}/bin/terraform init -upgrade -backend=false -input=false
    ${terraform}/bin/terraform validate | tee $out
  '';
}
