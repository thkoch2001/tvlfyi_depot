#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2023 The TVL Authors
#
# SPDX-License-Identifier: MIT

#
# Builds a NixOS system configuration at the given attribute path.
set -ueo pipefail

# Load input variables from Terraform. jq's @sh format takes care of
# escaping.
eval "$(jq -r '@sh "attrpath=\(.attrpath) && entrypoint=\(.entrypoint) && argstr=\((.argstr // {}) | to_entries | map ("--argstr", .key, .value) | join(" "))"')"

# Evaluate the system derivation.
[[ -z "$entrypoint" ]] && entrypoint=$(git rev-parse --show-toplevel)
# shellcheck disable=SC2086,SC2154
system_drv=$(nix-instantiate -A "${attrpath}" "${entrypoint}" ${argstr})

# Return system derivation back to Terraform.
jq -n --arg drv "$system_drv" '{"drv":$drv}'
