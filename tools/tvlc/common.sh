#!/bin/bash

set -eu
set -o pipefail

source path-scripts

XDG_DATA_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"
tvlc_root="$XDG_DATA_HOME/tvlc"

nice_checkout_root=
if [ -f "$tvlc_root"/nice_checkout_root ]; then
  nice_checkout_root="$(cat "$tvlc_root"/nice_checkout_root)"
fi
nice_checkout_root="${nice_checkout_root:-$HOME/tvlc}"

depot_root=
if [ -f "$tvlc_root/depot_root" ]; then
  depot_root="$(cat "$tvlc_root/depot_root")"
fi
if [ -d /depot ]; then
  # don't require config on tvl nixos servers
  depot_root="${depot_root:-/depot}"
fi
if [ -n "$depot_root" ]; then
  export DEPOT_ROOT="$depot_root"
fi

if [ ! -d "$tvlc_root" ]; then
  echo "tvlc: setup required"
  echo "please run 'tvlc setup' from the depot root"
  exit 1
fi
