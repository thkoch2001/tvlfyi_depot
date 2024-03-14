{ depot, pkgs, ... }:

let
  llvmPackages = pkgs.llvmPackages_11;
in

depot.third_party.naersk.buildPackage {
  src = ./.;

  buildInputs =
    [
      llvmPackages.clang
      llvmPackages.llvm
      llvmPackages.bintools
      llvmPackages.libclang.lib
    ]
    ++ (with pkgs; [
      zlib
      ncurses
      libxml2
      libffi
      pkg-config
    ]);

  doCheck = true;

  # Trouble linking against LLVM, maybe since rustc's llvmPackages got bumped?
  meta.ci.skip = true;
}
