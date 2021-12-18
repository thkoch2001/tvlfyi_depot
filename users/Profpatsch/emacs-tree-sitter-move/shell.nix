{ pkgs ? import ../../../third_party { }, ... }:
let
  inherit (pkgs) lib;

  treeSitterGrammars = pkgs.runCommandLocal "grammars" { } ''
    mkdir -p $out/bin
    ${lib.concatStringsSep "\n"
      (lib.mapAttrsToList (name: src: "ln -s ${src}/parser $out/bin/${name}.so") pkgs.tree-sitter.builtGrammars)};
  '';

in
pkgs.mkShell {
  buildInputs = [
    pkgs.tree-sitter.builtGrammars.python
  ];
  TREE_SITTER_GRAMMAR_DIR = treeSitterGrammars;
}
