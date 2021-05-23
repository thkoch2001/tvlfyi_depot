{ depot, pkgs, ... }:

(pkgs.grpc.override {
  protobuf = depot.third_party.protobuf;
  stdenv = pkgs.llvmPackages_11.libcxxStdenv;
  abseil-cpp = depot.third_party.abseil_cpp;
}).overrideAttrs(orig: rec {
  cmakeFlags = orig.cmakeFlags ++ [
    "-DCMAKE_CXX_STANDARD=17"
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON"
  ];
})
