# Utility script to run a gerrit command on the depot host via ssh.
# Reads the username from TVL_USERNAME, or defaults to $(whoami)
{ pkgs, ... }:

pkgs.writeShellScriptBin "gerrit" ''
  TVL_USERNAME=''${TVL_USERNAME:-$(whoami)}
  if which ssh &>/dev/null; then
    ssh=ssh
  else
    ssh="${pkgs.openssh}/bin/ssh"
  fi
  exec $ssh $TVL_USERNAME@code.tvl.fyi -p 29418 -- gerrit $@
''
