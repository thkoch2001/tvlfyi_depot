{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;

  buildInputs = with pkgs; [
    clang_11
    llvm_11.lib
    llvmPackages_11.bintools
    llvmPackages_11.clang
    llvmPackages_11.libclang.lib
    zlib
    ncurses
    libxml2
    libffi
    pkgconfig
  ];

  doCheck = true;
}
