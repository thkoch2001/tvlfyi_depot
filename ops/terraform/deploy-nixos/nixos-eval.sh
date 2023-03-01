#!/usr/bin/env bash
#
# Builds a NixOS system configuration at the given attribute path.
set -ueo pipefail

# Load input variables from Terraform. jq's @sh format takes care of
# escaping.
eval "$(jq -r '@sh "ATTRPATH=\(.attrpath) && ENTRYPOINT=\(.entrypoint)"')"

# Evaluate the system derivation.
[[ -z "$ENTRYPOINT" ]] && ENTRYPOINT=$(git rev-parse --show-toplevel)
SYSTEM_DRV=$(nix-instantiate -A "${ATTRPATH}" "${ENTRYPOINT}")

# Return system derivation back to Terraform.
jq -n --arg drv "$SYSTEM_DRV" '{"drv":$drv}'
