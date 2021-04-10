{ pkgs, ... }:

(pkgs.grpc.override {
  protobuf = pkgs.protobuf;
  stdenv = pkgs.llvmPackages.libcxxStdenv;
}).overrideAttrs(orig: rec {
  version = "1.30.0";

  src = pkgs.fetchFromGitHub {
    owner = "grpc";
    repo = "grpc";
    rev = "v${version}";
    sha256 = "01w5jdp318i7ncxbkfv75q0mf0rd3qwfr1ycdd3850nv6mprv7n0";
    fetchSubmodules = true;
  };

  cmakeFlags = orig.cmakeFlags ++ [
    "-DCMAKE_CXX_STANDARD=17"
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON"
  ];
})
