{ pkgs, ... }:

(pkgs.originals.grpc.override {
  protobuf = pkgs.protobuf;
  stdenv = pkgs.llvmPackages.libcxxStdenv;
  abseil-cpp = pkgs.abseil_cpp;
}).overrideAttrs(orig: rec {
  cmakeFlags = orig.cmakeFlags ++ [
    "-DCMAKE_CXX_STANDARD=17"
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON"
  ];
})
