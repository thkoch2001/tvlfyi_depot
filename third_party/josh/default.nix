# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "esrlabs";
  repo = "josh";
  rev = "1c1965deb0278de92452a597c092f16b531274ae";
  sha256 = "0a0ybx4dqr9gfcn1kqy2nirdsfbdwkfz24ljnri564c5381ccldw";
};
in depot.third_party.naersk.buildPackage {
  inherit src;
}
