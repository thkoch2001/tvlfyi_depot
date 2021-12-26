{ depot ? (import ../.. { })
, pkgs ? depot.third_party.nixpkgs
, ...
}:

# Ensure formatting is coherent,
# but do this in parallel to the main build because:
#  - (in favor of building this after tvix)
#    tests run so that developers get all the useful feedback
#  - (in favor of building this before tvix)
#    if the formatting is broken, and this build was submitted to CI
#    it would be a good idea to get this feedback rather sooner than later
#  - we don't want builds to differ between local and CI runs

let
  inherit (depot.third_party.nix) src;
in
pkgs.fullLlvm11Stdenv.mkDerivation {
  name = "tvix-checkfmt";
  inherit (depot.third_party.nix) src;

  nativeBuildInputs = [ pkgs.clang-tools_11 ];
  SANDBOX_SHELL = "${pkgs.busybox}/bin/busybox";

  buildPhase = ''
    set -e
    runHook preBuild
    fd . $src -e hh -e cc | xargs clang-format --dry-run --Werror
    touch $out
    runHook postBuild
  '';
}
