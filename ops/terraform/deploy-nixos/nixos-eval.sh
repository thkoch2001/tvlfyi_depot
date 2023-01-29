#!/usr/bin/env bash
#
# Builds a NixOS system configuration at the given attribute path.
set -ueo pipefail

# Load input variables from Terraform. jq's @sh format takes care of
# escaping.
eval "$(jq -r '@sh "CLOSURE=\(.closure)"')"

# Evaluate the system derivation.
# TODO: configurable REPO_ROOT
REPO_ROOT=$(git rev-parse --show-toplevel)
SYSTEM_DRV=$(nix-instantiate -A "${CLOSURE}" "${REPO_ROOT}")

# Return system derivation back to Terraform.
jq -n --arg drv "$SYSTEM_DRV" '{"drv":$drv}'
