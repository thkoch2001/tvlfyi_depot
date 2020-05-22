{ depot, pkgs, ... }:

let
  stdenv = with pkgs; overrideCC clangStdenv clang_10;
  abseil_cpp = pkgs.abseil_cpp.override { inherit stdenv; };
in stdenv.mkDerivation {
  name = "dt";
  src = ./.;
  nativeBuildInputs = [ pkgs.cmake ];
  buildInputs = with pkgs; [
    abseil_cpp
    farmhash
  ];
}
