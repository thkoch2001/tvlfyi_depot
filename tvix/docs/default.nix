{ pkgs, lib, ... }:

pkgs.stdenv.mkDerivation {
  pname = "tvix-docs";
  version = "0.1";

  outputs = [ "out" ];

  src = lib.cleanSource ./.;

  nativeBuildInputs = [
    pkgs.d2
    pkgs.mdbook
    pkgs.mdbook-admonish
    pkgs.mdbook-d2
    pkgs.mdbook-plantuml
    pkgs.plantuml
  ];

  # plantuml wants to create ./.mdbook-plantuml-cache, which fails as $src is r/o.
  # copy all sources elsewhere to workaround.
  buildCommand = ''
    cp -R $src/. .
    mdbook build -d $out
  '';

  meta.ci.extraSteps.deploy = {
    label = ":rocket: deploy docs";
    needsOutput = true;
    alwaysRun = true;
    prompt = "Deploy tvix-docs?";
    phase = "release";
    # branches = ["canon"];
    command = pkgs.writeShellScript "deploy-tvix-docs" ''
      RCLONE_WEBDAV_URL="https://api.garage.flokli.io/tvix-docs" \
      RCLONE_WEBDAV_BEARER_TOKEN_COMMAND="${pkgs.buildkite-agent}/bin/buildkite-agent oidc request-token --audience api.garage.flokli.io" \
      ${pkgs.rclone}/bin/rclone sync result/. :webdav:
    '';
  };
}
