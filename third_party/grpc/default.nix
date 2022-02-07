{ depot, pkgs, ... }:

(pkgs.grpc.override {
  protobuf = depot.third_party.protobuf;
  stdenv = pkgs.fullLlvm11Stdenv;
  re2 = depot.third_party.re2;
}).overrideAttrs (orig: rec {
  cmakeFlags = orig.cmakeFlags ++ [
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON"
    "-DCMAKE_CXX_STANDARD=17"
  ];
})
