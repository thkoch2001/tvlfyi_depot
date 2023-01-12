{ pkgs, ... }:

pkgs.writeShellScriptBin "hello-world" ''
  echo "Hello world"
''
