#!/usr/bin/env bash
#
# Copies a NixOS system to a target host, using the provided key,
# or whatever ambient key is configured if the key is not set.
set -ueo pipefail

export NIX_SSHOPTS="\
    -o StrictHostKeyChecking=no\
    -o UserKnownHostsFile=/dev/null\
    -o GlobalKnownHostsFile=/dev/null"

# If DEPLOY_KEY was passed, write it to $scratch/id_deploy
if [ -n "${DEPLOY_KEY-}" ]; then
  scratch="$(mktemp -d)"
  trap 'rm -rf -- "${scratch}"' EXIT

  echo -n "$DEPLOY_KEY" > $scratch/id_deploy
  chmod 0600 $scratch/id_deploy
  export NIX_SSHOPTS="$NIX_SSHOPTS -o IdentityFile=$scratch/id_deploy"
fi

nix-copy-closure \
  --to ${TARGET_USER}@${TARGET_HOST} \
  ${SYSTEM_DRV} \
  --gzip \
  --include-outputs \
  --use-substitutes
