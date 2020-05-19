{ pkgs, ... }:

(pkgs.originals.abseil-cpp.overrideAttrs(_: {
  version = "20200519-768eb2ca";

  src = pkgs.fetchFromGitHub {
    owner = "abseil";
    repo = "abseil-cpp";
    rev = "768eb2ca2857342673fcd462792ce04b8bac3fa3";
    sha256 = "13cyc5qm8n9zd60vcjwvyvlv237421nvhzrnw43y5w9pxshixn2q";
  };
}))
