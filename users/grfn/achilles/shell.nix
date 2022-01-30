with (import ../../.. { }).third_party.nixpkgs;

mkShell {
  buildInputs = [
    clang_11
    llvm_11.lib
    llvmPackages_11.bintools
    llvmPackages_11.clang
    llvmPackages_11.libclang.lib
    zlib
    ncurses
    libxml2
    libffi
    pkg-config
  ];

  LLVM_SYS_110_PREFIX = llvmPackages_11.bintools;
}
