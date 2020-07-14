#!/usr/bin/env bash
set -euo pipefail

CLANGD_FLAGS=--compile-commands-dir=/home/grfn/builds/tazjix \
    nix-shell /home/grfn/code/depot \
    -A third_party.nix \
    --run nix-clangd
