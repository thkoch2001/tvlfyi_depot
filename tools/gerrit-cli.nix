# Utility script to run a gerrit command on the depot host via ssh.
# Reads the username from TVL_USERNAME, or defaults to $(whoami)
{ pkgs, ... }:

pkgs.writeShellScriptBin "gerrit" ''
  TVL_USERNAME=''${TVL_USERNAME:-$(whoami)}
  ${pkgs.openssh}/bin/ssh $TVL_USERNAME@code.tvl.fyi -p 29418 -- gerrit $@
''
