{ depot, pkgs, ... }:

let
  stdenv = with pkgs; overrideCC clangStdenv clang_9;
  abseil-cpp = pkgs.abseil-cpp.override { inherit stdenv; };
in stdenv.mkDerivation {
  name = "dt";
  src = ./.;
  nativeBuildInputs = [ pkgs.cmake ];
  buildInputs = with pkgs; [
    abseil-cpp
    farmhash
  ];
}
