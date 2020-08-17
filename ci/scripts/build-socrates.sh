set -euo pipefail

nix-build /home/wpcarro/nixpkgs/nixos \
  -I nixos-config=/home/wpcarro/briefcase/nixos/socrates/default.nix \
  -A system \
  --show-trace
