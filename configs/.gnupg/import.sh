#!/usr/bin/env bash

set -e

# Run this script to import all of the information exported by `export.sh`.
# Usage: ./import.sh path/to/directory

gpg --import "$1/public.asc"
gpg --import "$1/secret.asc"
gpg --import-ownertrust "$1/ownertrust.txt"

# Run this at the end to output some verification
gpg --list-keys
