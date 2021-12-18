args@{ pkgs, depot, ... }:
with pkgs;
let
  site = import ./site.nix args;
  resume = import ../resume args;
  bucket = "s3://gws.fyi";
  distributionID = "E2ST43JNBH8C64";

  css = runCommand "main.css"
    {
      buildInputs = [ pkgs.minify ];
    } ''
    minify --type css < ${./main.css} > $out
  '';

  keys = runCommand "ssh-keys" { } ''
    touch $out
    echo "${depot.users.grfn.keys.main}" >> $out
  '';

  website =
    runCommand "gws.fyi" { } ''
      mkdir -p $out
      cp ${css} $out/main.css
      cp ${site.index} $out/index.html
      cp -r ${site.recipes} $out/recipes
      cp ${resume} $out/resume.pdf
      cp ${keys} $out/keys
    '';

in
(writeShellScript "deploy.sh" ''
  ${awscli2}/bin/aws --profile personal s3 sync ${website}/ ${bucket}
  echo "Deployed to http://gws.fyi"
'') // {
  inherit website site;
}
