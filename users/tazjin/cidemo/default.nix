{ pkgs, ... }:

pkgs.buildGoPackage {
  name = "cidemo";
  goPackagePath = "tvl.fyi/users/tazjin/cidemo";
  src = ./.;
}
