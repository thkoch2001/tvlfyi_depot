#!/usr/bin/env bash
set -euo pipefail

# Setup a store for local development rooted at the current directory, and
# compatible with the scripts in this directory (repl.sh, build.sh, eval.sh,
# daemon.sh, etc)

mkdir -p nix/store nix/var/nix nix/var/log/nix
ln -s $(pwd)/src/nix ./nix/build-remote
mkdir -p $(dirname "$(pwd)${SANDBOX_SHELL}")
cp "${SANDBOX_SHELL}" "$(pwd)${SANDBOX_SHELL}"
