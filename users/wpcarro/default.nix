{ depot, pkgs, lib, ... }:

let
  inherit (builtins) fetchGit readDir path;
  inherit (pkgs.lib) filterAttrs mapAttrs;
  inherit (pkgs.lib.strings) hasPrefix;

  readBriefcase = briefcase: path: depot.nix.readTree {
    inherit path;

    args = {
      inherit depot pkgs briefcase;
    };

    scopedArgs = {
      inherit __findFile;
    };
  };

in lib.fix(self: mapAttrs
  (name: _: readBriefcase self (./. + "/${name}"))
  (filterAttrs
    (name: type: type == "directory" && !hasPrefix "." name)
    (readDir ./.)))
