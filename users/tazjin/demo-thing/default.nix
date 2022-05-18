{ pkgs, ... }:

pkgs.writeShellScript "example" ''
  echo Hello world!
''
