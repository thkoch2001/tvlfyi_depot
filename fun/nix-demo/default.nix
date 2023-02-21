{ pkgs, ... }:

pkgs.writeShellScript "nix-demo" ''
  echo "Hello world!"
''
