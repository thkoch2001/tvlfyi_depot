set -euo pipefail

nix-build . \
  -I briefcase="$(pwd)" \
  -I nixpkgs=/var/lib/buildkite-agent-socrates/nixpkgs-channels \
  --show-trace
