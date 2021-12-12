{ pkgs, ... }:

let
  gitignoreNix = import (pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "f9e996052b5af4032fe6150bba4a6fe4f7b9d698";
    sha256 = "0jrh5ghisaqdd0vldbywags20m2cxpkbbk5jjjmwaw0gr8nhsafv";
  }) { inherit (pkgs) lib; };

in {
  __functor = _: gitignoreNix.gitignoreSource;

  # expose extra functions here
  inherit (gitignoreNix) gitignoreFilter;
}
