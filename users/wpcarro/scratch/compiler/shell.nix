{ pkgs, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    ocaml
    ocamlPackages.utop
    ocamlformat
  ];
}
