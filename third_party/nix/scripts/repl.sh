#!/usr/bin/env bash
set -eo pipefail

# Run `nix repl` using a local store, for use during development. Intended to
# be run from the cmake build directory

if [ "$#" -gt 0 ] && [ "$1" = "--debug" ]; then
    gdb=(gdb --args)
    shift 1
elif [ "$1" = "--rr" ]; then
    gdb=(rr record)
    shift 1
else
    gdb=()
fi

make -j 10
NIX_STORE_DIR=$(pwd)/nix/store \
    NIX_LOG_DIR=$(pwd)/nix/var/log/nix \
    NIX_STATE_DIR=$(pwd)/nix/var/nix \
    XDG_CACHE_HOME=$(pwd)/cache \
    NIX_REMOTE=daemon \
    ${gdb[*]} ./src/nix repl "$@"
