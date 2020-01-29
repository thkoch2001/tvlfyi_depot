{ pkgs ? import <nixpkgs> {}, ... }:

{ name, deps, src }:

let
  inherit (pkgs) pythonPackages writeTextFile;
  inherit (builtins) toFile;

in writeTextFile {
  inherit name;
  executable = true;
  destination = "/bin/${name}";

  text = ''
    #!/bin/sh
    ${pkgs.python3}/bin/python3 ${src}
  '';
}
