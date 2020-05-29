{ pkgs, lib, ... }:

let inherit (pkgs) llvmPackages cmake;
in llvmPackages.libcxxStdenv.mkDerivation {
  name = "glog";
  version = "20200527-unstable";
  src = ./.;

  nativeBuildInputs = [ cmake ];
  cmakeFlags = [ "-WITH_GFLAGS=OFF" ];

  meta = with lib; {
    homepage = "https://github.com/google/glog";
    license = licenses.bsd3;
    description = "Library for application-level logging";
    platforms = platforms.unix;
  };
}
