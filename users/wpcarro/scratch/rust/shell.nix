{
  pkgs ? import <nixpkgs> { },
  ...
}:

pkgs.mkShell { buildInputs = [ pkgs.cargo ]; }
