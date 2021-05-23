# Pin protobuf to version 3.12, with LLVM.
{ depot, pkgs, ... }:

pkgs.callPackage "${pkgs.path}/pkgs/development/libraries/protobuf/generic-v3.nix" {
  version = "3.12.2";
  sha256 = "1lp368aa206vpic9fmax4k6llnmf28plfvkkm4vqhgphmjqykvl2";
  stdenv = pkgs.llvmPackages_11.libcxxStdenv;
  buildPackages = {
    inherit (pkgs) which;
    stdenv = pkgs.llvmPackages_11.libcxxStdenv;
  };
}
