# Most of the Nix expressions in this folder are NixOS modules, which
# are not readTree compatible.
#
# Some things (such as system configurations) are, and we import them
# here manually.
#
# TODO(tazjin): Find a more elegant solution for the whole module
# situation.
{ ... }@args:

{
  whitby = import ./whitby/default.nix args;
}
