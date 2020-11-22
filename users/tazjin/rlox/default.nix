{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;
}
