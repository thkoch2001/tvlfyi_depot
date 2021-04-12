{ depot, pkgs, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;

  nativeBuildInputs = with pkgs.pkgsBuildTarget; [
    llvmPackages.clang
    llvmPackages.bintools
    pkgconfig
  ];

  buildInputs = with pkgs; [
    llvmPackages.llvm
    llvmPackages.libclang.lib
    zlib
    ncurses
    libxml2
    libffi
  ];

  doCheck = true;
}
