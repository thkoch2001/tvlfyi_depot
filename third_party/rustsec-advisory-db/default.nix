# RustSec's advisory db for crates
#
# Update using:
#
#   nix-prefetch-git --quiet --url https://github.com/RustSec/advisory-db.git > third_party/rustsec-advisory-db/pin.json
#
# TODO(Profpatsch): automatically update in regular intervals
{ pkgs, ... }:

let
  pin = builtins.fromJSON (builtins.readFile ./pin.json);

  date = builtins.head (builtins.split "T" pin.date);

in pkgs.fetchFromGitHub {
  name = "advisory-db-${date}";
  owner = "RustSec";
  repo = "advisory-db";
  inherit (pin) rev sha256;
}
