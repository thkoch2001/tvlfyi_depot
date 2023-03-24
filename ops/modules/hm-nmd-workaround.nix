{ lib, ... }:

{
  # Building the manual requires nmd which at the moment uses
  # the --store dummy:// flag unconditionally which is only
  # supported in Nix >= 2.4.
  config.manual = {
    manpages.enable = lib.mkForce false;
    json.enable = lib.mkForce false;
    html.enable = lib.mkForce false;
  };
}
