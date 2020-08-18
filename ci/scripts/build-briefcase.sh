set -euo pipefail

nix-build . \
  -I briefcase="$(pwd)" \
  --no-out-link \
  --show-trace
