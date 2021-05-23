{ pkgs, ... }:

(pkgs.callPackage "${pkgs.path}/pkgs/development/libraries/rapidcheck" {
  stdenv = pkgs.llvmPackages_11.libcxxStdenv;
}).overrideAttrs (attrs: rec {
  # follows the versioning scheme of nixpkgs, since rapidcheck does not
  # provide versioned releases
  version = "unstable-2020-05-04";

  src = pkgs.fetchFromGitHub {
    owner = "emil-e";
    repo = "rapidcheck";
    rev = "7bc7d302191a4f3d0bf005692677126136e02f60";
    sha256 = "0khawy2n007yk97ls2qqpna4ly09v6rb6hw72nm16kzk3zbyzh17";
  };

  cmakeFlags = [
    "-DRC_ENABLE_GTEST=ON"
    "-DRC_ENABLE_GMOCK=ON"
  ];
})
