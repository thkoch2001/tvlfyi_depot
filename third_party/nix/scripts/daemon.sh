#!/usr/bin/env bash
set -eo pipefail

# Run a nix daemon using a local store, for use during development. Intended to
# be run from the cmake build directory

if [ $1 = "--debug" ]; then
    run=(gdb --args)
    shift 1
elif [ "$1" = "--rr" ]; then
    run=(rr record)
    shift 1
else
    run=()
fi

make -j 10
NIX_STORE_DIR=$(pwd)/nix/store \
    NIX_LOG_DIR=$(pwd)/nix/var/log/nix \
    NIX_STATE_DIR=$(pwd)/nix/var/nix \
    XDG_CACHE_HOME=$(pwd)/cache \
    NIX_LIBEXEC_DIR=$(pwd) \
    GRPC_TRACE=all \
    ${gdb[*]} ./src/nix-daemon/nix-daemon
