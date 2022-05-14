{ depot, pkgs, ... }:

let
  llvmPackages = pkgs.llvmPackages_14;
in

depot.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs = [
    llvmPackages.clang
    llvmPackages.llvm
    llvmPackages.libllvm.dev
    llvmPackages.bintools
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
