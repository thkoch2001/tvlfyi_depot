#!/usr/bin/env bash

set -e

# Run this script to export all the information required to transport your GPG
# information.
# Usage: ./export.sh [directory]
# TODO: run this periodically as a job.

destination="${1:-$(mktemp -d)}"

if [ ! -d "${destination}" ]; then
  echo "${destination} does not exist. Creating it..."
  mkdir -p "${destination}"
fi

gpg --armor --export >"${destination}/public.asc"
gpg --armor --export-secret-keys >"${destination}/secret.asc"
gpg --armor --export-ownertrust >"${destination}/ownertrust.txt"

zip -r "${destination}.zip" "${destination}"
rm -rf "${destination}"

echo $(realpath "${destination}.zip")
