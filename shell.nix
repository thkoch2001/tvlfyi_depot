{ pkgs, ... }:

pkgs.mkShell rec {
  buildInputs = [];
  # TODO(wpcarro): How does pkgs.mkShell handle exported and non-exported
  # variable definitions?
  BRIEFCASE = builtins.toPath ~/briefcase;
  DEPOT = builtins.toPath ~/depot;
  NIXPKGS = builtins.toPath ~/nixpkgs;
  NIX_PATH="nixpkgs=${NIXPKGS}:depot=${DEPOT}:briefcase=${BRIEFCASE}";
  DESKTOP = "zeno.lon.corp.google.com";
  LAPTOP = "seneca";
}
