#! /usr/bin/env bash
set -euo pipefail

drv=$(nix-instantiate '<nixpkgs>' -A ghc)
nix --extra-experimental-features nix-command show-derivation -r "$drv" | jq -r '.[] | .outputs[].path, .inputSrcs[]' | sort -u | cut -d/ -f4 | cut -d- -f1 > maxrefs
nix-store --dump "$(nix-build "$drv")" > nar
