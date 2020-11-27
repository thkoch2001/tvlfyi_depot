{ depot, pkgs, ... }:

let
  stdenv = with pkgs; overrideCC clangStdenv clang_11;
  abseil_cpp = pkgs.abseil_cpp;
in stdenv.mkDerivation {
  name = "dt";
  src = ./.;
  nativeBuildInputs = [ pkgs.cmake ];
  buildInputs = with pkgs; [
    abseil_cpp
    farmhash
  ];
  meta.ci = false;
}
