args@{ pkgs, ... }:
with pkgs;
let
  site = import ./site.nix args;
  resume = import ../resume args;
  bucket = "s3://gws.fyi";
  distributionID = "E2ST43JNBH8C64";

  css = runCommand "main.css" {
    buildInputs = [ pkgs.minify ];
  } ''
    minify --type css < ${./main.css} > $out
  '';

  website =
    runCommand "gws.fyi" { } ''
      mkdir -p $out
      cp ${css} $out/main.css
      cp ${site.index} $out/index.html
      cp -r ${site.recipes} $out/recipes
      cp ${resume} $out/resume.pdf
    '';

in (writeShellScript "deploy.sh" ''
  ${awscli2}/bin/aws --profile personal s3 sync ${website}/ ${bucket}
  echo "Deployed to http://gws.fyi"
'') // {
  inherit website site;
}
