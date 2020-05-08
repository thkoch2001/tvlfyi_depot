# Google's farmhash family of hash functions
{ pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "farmhash";

  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "farmhash";
    rev = "0d859a811870d10f53a594927d0d0b97573ad06d";
    sha256 = "1w2583m5289hby0r91gds5yia6l8qpmzkl5b9bv58g5gacfj2h17";
  };
}
