with (import ../../.. { }).third_party.nixpkgs;

mkShell {
  buildInputs = [
    clang_14
    llvm_14.lib
    llvmPackages_14.bintools
    llvmPackages_14.clang
    llvmPackages_14.libclang.lib
    zlib
    ncurses
    libxml2
    libffi
    pkg-config
  ];

  LLVM_SYS_140_PREFIX = llvmPackages_14.libllvm.dev;
}
