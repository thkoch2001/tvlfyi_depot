# https://github.com/esrlabs/josh
{ depot, pkgs, ... }:

let src = pkgs.fetchFromGitHub {
  owner = "esrlabs";
  repo = "josh";
  rev = "1c1965deb0278de92452a597c092f16b531274ae";
  sha256 = pkgs.lib.fakeSha256;
};
in depot.third_party.naersk.buildPackage {
  inherit src;
}
