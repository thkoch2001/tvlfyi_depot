{ pkgs, ... }:

(pkgs.originals.gtest.override {
  stdenv = pkgs.llvmPackages.libcxxStdenv;
}).overrideAttrs(_: {
  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "googletest";
    rev = "a781fe29bcf73003559a3583167fe3d647518464";
    sha256 = "0zny8kgbkslazzsnskygj3pkcj7l13xhgcwjyxswxymxq8m41kgy";
  };
})
