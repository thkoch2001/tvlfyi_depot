# EXWM is present in nixpkgs and we do not (currently) intend to
# change the code structure, so the existing drv can be reused.
{ pkgs, ... }:

pkgs.emacsPackages.exwm.overrideAttrs(_: {
  src = ./.;
})
