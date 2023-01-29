#!/usr/bin/env bash
#
# Copies a NixOS system to a target host, using the provided key.
set -ueo pipefail

scratch="$(mktemp -d)"
trap 'rm -rf -- "${scratch}"' EXIT

echo -n "$DEPLOY_KEY" > $scratch/id_deploy
chmod 0600 $scratch/id_deploy

export NIX_SSHOPTS="\
    -o StrictHostKeyChecking=no\
    -o UserKnownHostsFile=/dev/null\
    -o GlobalKnownHostsFile=/dev/null\
    -o IdentityFile=$scratch/id_deploy"

nix-copy-closure \
  --to ${TARGET_USER}@${TARGET_ADDRESS} \
  ${SYSTEM_DRV} \
  --gzip \
  --include-outputs \
  --use-substitutes
