# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "esrlabs";
    repo = "josh";
    rev = "c56b1b81b66a43c67f94530bcb4979c1a9307c97";
    sha256 = "0vahfiv7min1carlhzljn4f4xv9v9fxxq5y8qwwldg7agxck3x6m";
  };
in depot.third_party.naersk.buildPackage {
  inherit src;

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkgconfig
  ];

  cargoBuildOptions = x: x ++ [
    "-p" "josh"
    "-p" "josh-proxy"
    "-p" "josh-ui"
  ];

  overrideMain = x: {
    patches = [ ./0001-replace-mentions-of-master-with-canon.patch ];
  };
}
