#!/usr/bin/env bash

set -euo pipefail

# Run this script to import all of the information exported by `export.sh`.
# Usage: ./import.sh path/to/export.zip

if [ -z "${1+x}" ]; then
  echo "You must specify the path to export.zip. Exiting..."
  exit 1
fi

destination="$(mktemp -d)"

function cleanup() {
  rm -rf "${destination}"
}
trap cleanup EXIT

unzip "${1}" -d "${destination}" >/dev/null

gpg --import "${destination}/public.asc"
gpg --import "${destination}/secret.asc"
gpg --import-ownertrust "${destination}/ownertrust.txt"

# Run this at the end to output some verification
gpg --list-keys
gpg --list-secret-keys
