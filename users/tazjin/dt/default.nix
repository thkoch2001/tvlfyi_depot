{ depot, pkgs, ... }:

let stdenv = with pkgs; overrideCC clangStdenv clang_11;
in
stdenv.mkDerivation {
  name = "dt";
  src = ./.;
  nativeBuildInputs = [ pkgs.cmake ];
  buildInputs = with depot.third_party; [
    abseil_cpp
    farmhash
  ];
  meta.ci = false;
}
