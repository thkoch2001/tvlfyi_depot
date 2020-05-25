{ pkgs, ... }:

pkgs.callPackage (pkgs.fetchFromGitHub {
  owner = "nmattia";
  repo = "naersk";
  rev = "a82fd7dc31a58c462b6dfa9d9d886fa2cc75dfd4";
  sha256 = "00bjwir52y6jbf0b22qy9qxramw35k5fc7kp9hymr1zgpmw9kbwg";
}) {
  lndir = pkgs.xorg.lndir;
}
