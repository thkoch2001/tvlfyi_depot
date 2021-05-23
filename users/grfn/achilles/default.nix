{ depot, pkgs, ... }:

let
  llvmPackages = pkgs.llvmPackages_11;
in

depot.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs = [
    llvmPackages.clang
    llvmPackages.llvm
    llvmPackages.bintools
    llvmPackages.clang
    llvmPackages.libclang.lib
  ] ++ (with pkgs; [
    zlib
    ncurses
    libxml2
    libffi
    pkgconfig
  ]);

  doCheck = true;
}
