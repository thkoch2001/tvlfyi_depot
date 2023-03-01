#!/usr/bin/env bash
#
# Builds a NixOS system configuration at the given attribute path.
set -ueo pipefail

# Load input variables from Terraform
eval "$(jq -r '@sh "CLOSURE=\(.closure) && ENTRYPOINT=\(.entrypoint)"')"

# Evaluate the system derivation.
[[ -z "$ENTRYPOINT" ]] && ENTRYPOINT=$(git rev-parse --show-toplevel)
SYSTEM_DRV=$(nix-instantiate -A "${CLOSURE}" "${ENTRYPOINT}")

# Return system derivation back to Terraform.
jq -n --arg drv "$SYSTEM_DRV" '{"drv":$drv}'
