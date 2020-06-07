{ pkgs, ... }:

(pkgs.originals.grpc.override {
  protobuf = pkgs.protobuf;
  stdenv = pkgs.llvmPackages.libcxxStdenv;
}).overrideAttrs(orig: {
  buildInputs = orig.buildInputs ++ [
    pkgs.abseil_cpp
  ];

  cmakeFlags = orig.cmakeFlags ++ [
    "-DgRPC_ABSL_PROVIDER=package"
  ];
})
