{ pkgs, lib, ... }:

let inherit (pkgs) llvmPackages_11 cmake;
in llvmPackages_11.libcxxStdenv.mkDerivation {
  name = "glog";
  version = "20200527-unstable";
  src = ./.;

  nativeBuildInputs = [ cmake ];
  cmakeFlags = [
    "-DCMAKE_CXX_STANDARD=17"
    "-WITH_GFLAGS=OFF"
  ];

  meta = with lib; {
    homepage = "https://github.com/google/glog";
    license = licenses.bsd3;
    description = "Library for application-level logging";
    platforms = platforms.unix;
  };
}
