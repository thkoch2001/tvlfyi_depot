{ pkgs, ... }:

(pkgs.gtest.override { stdenv = pkgs.fullLlvm11Stdenv; }).overrideAttrs (_: {
  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "googletest";
    rev = "9dce5e5d878176dc0054ef381f5c6e705f43ef99";
    sha256 = "05xi61j7j251dzkgk9965lqpbacsy44iblzql941kw9d4nk0q6jl";
  };
})
