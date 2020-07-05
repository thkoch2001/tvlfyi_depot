{ pkgs, ... }:

(pkgs.originals.grpc.override {
  abseil-cpp = pkgs.abseil_cpp;
  protobuf = pkgs.protobuf;
  stdenv = pkgs.llvmPackages.libcxxStdenv;
}).overrideAttrs(orig: {
  cmakeFlags = orig.cmakeFlags ++ [
    "-DCMAKE_CXX_STANDARD=17"
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON"
  ];
})
