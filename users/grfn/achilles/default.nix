{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs = with pkgs; [
    clang_11
    llvmPackages.llvm
    llvmPackages.bintools
    llvmPackages.clang
    llvmPackages.libclang.lib
    zlib
    ncurses
    libxml2
    libffi
    pkgconfig
  ];

  doCheck = true;
}
