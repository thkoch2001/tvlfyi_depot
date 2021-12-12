# Portable URI library
{ depot, pkgs, ... }:

let
  src = pkgs.fetchgit {
    url = "http://git.kpe.io/puri.git";
    rev = "4bbab89d9ccbb26346899d1f496c97604fec567b";
    sha256 = "0gq2rsr0aihs0z20v4zqvmdl4szq53b52rh97pvnmwrlbn4mapmd";
  };
in depot.nix.buildLisp.library {
  name = "puri";
  srcs = [ (src + "/src.lisp") ];
}
