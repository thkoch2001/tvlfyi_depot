let
  briefcase = import <briefcase> {};
in briefcase.utils.nixBufferFromShell ./shell.nix
