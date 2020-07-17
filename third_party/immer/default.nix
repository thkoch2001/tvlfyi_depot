{ pkgs, lib, ... }:

pkgs.llvmPackages.libcxxStdenv.mkDerivation rec {
  name = "immer-git";
  version = "git";
  src = ./.;
  nativeBuildInputs = [ pkgs.cmake ];
  dontBuild = true;

  cmakeFlags = [
    "-DIMMER_HAS_LIBGC=1"
    "-DIMMER_CXX_STANDARD=17"
  ];

  meta = with lib; {
    homepage    = "https://github.com/arximboldi/immer";
    description = "Postmodern immutable data structures for C++";
    license     = licenses.boost;
  };
}
