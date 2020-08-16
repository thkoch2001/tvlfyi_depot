set -euo pipefail

nixos-rebuild \
  -I nixos-config=/home/wpcarro/briefcase/nixos/socrates/configuration.nix \
  -I nixpkgs=/home/wpcarro/nixpkgs \
  -I depot=/home/wpcarro/depot \
  -I briefcase=/home/wpcarro/briefcase \
  switch
