args@{ pkgs, depot, ... }:

let
  orgExportHTML = import ./orgExportHTML.nix args;
in

{
  index = orgExportHTML ./index.org;
}
