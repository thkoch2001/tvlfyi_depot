#!/usr/bin/env bash

set -euo pipefail

# Run this script to export all the information required to transport your GPG
# information.
# Usage: ./export.sh
# TODO: run this periodically as a job.

output="$(pwd)/export.zip"
destination="$(mktemp -d)"

function cleanup() {
  rm -rf "${destination}"
}
trap cleanup EXIT

gpg --armor --export >"${destination}/public.asc"
gpg --armor --export-secret-keys >"${destination}/secret.asc"
gpg --armor --export-ownertrust >"${destination}/ownertrust.txt"

# Strangely enough this appears to be the only way to create a zip of a
# directory that doesn't contain the (noisy) full paths of each item from the
# source filesystem. (i.e. -j doesn't cooperate with -r)
pushd "${destination}"
zip -r "${output}" ./*
popd

echo "$(realpath ${output})"
