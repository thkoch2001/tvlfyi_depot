{ pkgs, ... }:

let
  pythonEnv = pkgs.python3.withPackages (ps: with ps; [
    mkdocs
    mkdocs-material
    pillow
    cairosvg
  ]);
in
pkgs.runCommand "website"
{
  buildInputs = [
    pythonEnv
  ];
}
  ''
    cp -r ${./.} ./source
    chmod -R +w ./source
    cd ./source
    mkdocs build
    mv site $out
  ''
