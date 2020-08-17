set -euo pipefail

nix-build . \
  -I briefcase="$(pwd)" \
  -I nixpkgs=/home/wpcarro/nixpkgs-channel \
  --show-trace
