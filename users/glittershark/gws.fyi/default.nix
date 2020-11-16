args@{ pkgs, ... }:
with pkgs;
let
  site = import ./site.nix args;
  resume = import ../resume args;
  bucket = "s3://gws.fyi";
  distributionID = "E2ST43JNBH8C64";
  website =
    runCommand "gws.fyi" { } ''
      mkdir -p $out
      cp ${site.index} $out/index.html
      cp ${resume} $out/resume.pdf
    '';

in writeShellScript "deploy.sh" ''
  ${awscli}/bin/aws s3 sync ${website}/ ${bucket}
  ${awscli}/bin/aws cloudfront create-invalidation \
    --distribution-id "${distributionID}" \
    --paths "/*"
  echo "Deployed to http://gws.fyi"
''
