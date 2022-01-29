{ pkgs, ... }:

pkgs.callPackage
  (pkgs.fetchFromGitHub {
    owner = "nmattia";
    repo = "naersk";
    rev = "a3f40fe42cc6d267ff7518fa3199e99ff1444ac4";
    sha256 = "1nf7fn8anghwf6p5p58ywbcwdkjxq112qv663rn52jq9k95iakdi";
  })
{ }
