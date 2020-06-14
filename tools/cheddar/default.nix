{ pkgs, ... }:

pkgs.naersk.buildPackage {
  src = ./.;
  doDoc = false;
  doCheck = false;

  override = x: {
    # Use our custom bat syntax set, which is everything from upstream,
    # plus additional languages we care about.
    BAT_SYNTAXES = "${pkgs.bat_syntaxes}";

    # LLVM packages (why are they even required?) are not found
    # automatically if added to buildInputs, hence this ...
    LIBCLANG_PATH = "${pkgs.llvmPackages.libclang}/lib/libclang.so.10";
  };
}
