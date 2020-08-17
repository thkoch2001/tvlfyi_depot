set -euo pipefail

nix-build '<nixpkgs/nixos>' \
  -I briefcase="$(pwd)" \
  -I nixpkgs=/home/wpcarro/nixpkgs-channels \
  -I nixos-config=nixos/socrates/default.nix \
  -A system \
  --show-trace
