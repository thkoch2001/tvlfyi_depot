{ config, lib, pkgs, ... }:

with lib;

{
  options = {
    programs.zsh.functions = mkOption {
      description = "An attribute set that maps function names to their source";
      default = {};
      type = with types; attrsOf (either str path);
    };
  };

  config.programs.zsh.initExtra = concatStringsSep "\n" (
    mapAttrsToList (name: funSrc: ''
      function ${name}() {
        ${funSrc}
      }
    '') config.programs.zsh.functions
  );
}
