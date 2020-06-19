{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;
  doDoc = false;
  doCheck = false;

  override = x: rec {
    # Use our custom bat syntax set, which is everything from upstream,
    # plus additional languages we care about.
    BAT_SYNTAXES = "${pkgs.bat_syntaxes}";

    # LLVM packages (why are they even required?) are not found
    # automatically if added to buildInputs, hence this ...
    LIBCLANG_PATH = "${pkgs.llvmPackages.libclang}/lib/libclang.so.10";

    shellHook = ''
      export BAT_SYNTAXES=${BAT_SYNTAXES}
      export LIBCLANG_PATH=${LIBCLANG_PATH}
    '';
  };
}
