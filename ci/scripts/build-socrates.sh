set -euo pipefail

nix-build /home/wpcarro/nixpkgs/nixos \
  -I briefcase="$(pwd)" \
  -I nixos-config=nixos/socrates/default.nix \
  -A system \
  --show-trace
