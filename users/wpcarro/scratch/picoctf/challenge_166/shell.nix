{ pkgs, ... }:

let
  python = pkgs.python3.withPackages (pypkgs: with pypkgs; [ cryptography ]);
in
python.env
