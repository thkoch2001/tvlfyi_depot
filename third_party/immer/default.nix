{ pkgs, lib, ... }:

pkgs.llvmPackages.libcxxStdenv.mkDerivation rec {
  name = "immer-git";
  version = "git";
  src = ./.;
  nativeBuildInputs = [ pkgs.cmake ];
  dontBuild = true;
  meta = with lib; {
    homepage    = "https://github.com/arximboldi/immer";
    description = "Postmodern immutable data structures for C++";
    license     = licenses.boost;
  };
}
