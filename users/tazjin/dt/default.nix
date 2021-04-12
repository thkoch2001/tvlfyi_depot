{ depot, pkgs, ... }:

pkgs.clangStdenv.mkDerivation {
  name = "dt";
  src = ./.;
  nativeBuildInputs = [ pkgs.buildPackages.cmake ];
  buildInputs = with depot.third_party; [
    abseil_cpp
    farmhash
  ];
  meta.ci = false;
}
