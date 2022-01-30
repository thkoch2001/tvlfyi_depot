# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let
  src = pkgs.fetchFromGitHub {
    owner = "esrlabs";
    repo = "josh";
    rev = "69dc986e506ba5631c8bbf52835da076a18ec8dc";
    hash = "sha256:0ybc6ivjkm7bchaszs9lhbl1gbjnyhwq7a3vw6jml3ama84l52lb";
  };
in
depot.third_party.naersk.buildPackage {
  inherit src;

  buildInputs = with pkgs; [
    libgit2
    openssl
    pkgconfig
  ];

  cargoBuildOptions = x: x ++ [
    "-p"
    "josh"
    "-p"
    "josh-proxy"
    "-p"
    "josh-ui"
  ];
}
