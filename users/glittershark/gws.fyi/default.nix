with import <nixpkgs> {};
let
  bucket = "s3://gws.fyi";
  distributionID = "E2ST43JNBH8C64";
  website =
    runCommand "gws.fyi" { } ''
      mkdir -p $out
      cp ${./index.html} $out/index.html
    '';
in writeShellScript "deploy.sh" ''
  ${awscli}/bin/aws s3 sync ${website}/ ${bucket}
  ${awscli}/bin/aws cloudfront create-invalidation \
    --distribution-id "${distributionID}" \
    --paths "/*"
  echo "Deployed to http://gws.fyi"
''
