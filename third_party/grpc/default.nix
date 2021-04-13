{ depot, pkgs, ... }:

(pkgs.grpc.override {
  protobuf = depot.third_party.protobuf;
  stdenv = pkgs.llvmPackages.libcxxStdenv;
  abseil-cpp = depot.third_party.abseil_cpp;
  re2 = pkgs.re2.overrideAttrs (old: {
    # TODO(sterni): update upstream, so this isn't necessary
    version = "20210401";
    src = pkgs.fetchFromGitHub {
      owner = "google";
      repo = "re2";
      rev = "2021-04-01";
      sha256 = "1iia0883lssj7ckbsr0n7yb3gdw24c8wnl2q5hhzlml23h4ipbh3";
    };
  });
}).overrideAttrs(orig: rec {
  cmakeFlags = orig.cmakeFlags ++ [
    "-DCMAKE_CXX_STANDARD=17"
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON"
  ];
})
