{ pkgs ? import <nixpkgs> {}, ... }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    dune_3
    ocaml
    ocamlPackages.utop
    ocamlformat
    ocamlPackages.yojson
    ocamlPackages.findlib
    ocamlPackages.ppx_yojson_conv_lib
    ocamlPackages.ppx_jane
  ];
}
