{ depot, ... }:

let
  mkShell = import "${depot.third_party.nixpkgsSrc}/pkgs/build-support/mkshell/default.nix" {
    lib = depot.third_party.lib;
    stdenv = depot.third_party.llvmPackages.libcxxStdenv;
  };
in
mkShell {
  nativeBuildInputs = [
    depot.third_party.nix
    depot.third_party.pkgconfig
  ];
}
