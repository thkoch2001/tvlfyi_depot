args@{ pkgs ? import <nixpkgs> { }, ... }:

let

  orgExportHTML = import ./orgExportHTML.nix args;

in

{
  index = orgExportHTML ./index.org;
  recipes = orgExportHTML ./recipes;
}
