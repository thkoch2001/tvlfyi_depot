#!/usr/bin/env bash

# SPDX-FileCopyrightText: 2023 The TVL Authors
#
# SPDX-License-Identifier: MIT
set -ueo pipefail

# Evaluates a Nix expression.
#
# Receives input parameters as JSON from stdin.
# It expects a dict with the following keys:
#
#  - `attrpath`: the attribute.path pointing to the expression to instantiate.
#    Required.
#  - `entrypoint`: the path to the Nix file to invoke.
#    Optional. If omitted, will shell out to git to determine the repo root,
#    and Nix will use `default.nix` in there.
#  - `argstr`: A map containing string keys and values
#    which are passed to Nix as `--argstr $key $value`
#    command line args. Optional.
#
# jq's @sh format takes care of escaping.
eval "$(jq -r '@sh "attrpath=\(.attrpath) && entrypoint=\(.entrypoint) && argstr=\((.argstr // {}) | to_entries | map ("--argstr", .key, .value) | join(" "))"')"

# Evaluate the expression.
[[ -z "$entrypoint" ]] && entrypoint=$(git rev-parse --show-toplevel)
# shellcheck disable=SC2086,SC2154
drv=$(nix-instantiate -A "${attrpath}" "${entrypoint}" ${argstr})

# Return a JSON back to stdout.
# It contains the following keys:
#
# - `drv`: the store path of the Derivation that has been instantiated.
jq -n --arg drv "$drv" '{"drv":$drv}'
