# RustSec's advisory db for crates
{ pkgs, depot, ... }:

let
  inherit (depot.third_party.sources) rustsec-advisory-db;
in

pkgs.fetchFromGitHub {
  inherit (rustsec-advisory-db)
    owner
    repo
    sha256
    rev
    ;

  passthru = {
    inherit (rustsec-advisory-db) rev;
  };
}
