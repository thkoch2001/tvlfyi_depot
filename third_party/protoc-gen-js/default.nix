{ pkgs, ... }:

pkgs.callPackage ./package.nix {
  protobuf = pkgs.protobuf3_21;
}
