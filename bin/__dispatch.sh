#!/usr/bin/env bash
# This script dispatches invocations transparently to programs instantiated from
# Nix.
#
# To add a new tool, insert it into the case statement below by setting `attr`
# to the key in nixpkgs which represents the program you want to run.
set -ueo pipefail

readonly REPO_ROOT=$(dirname "$0")/..
TARGET_TOOL=$(basename "$0")

case "${TARGET_TOOL}" in
  age)
    attr="third_party.nixpkgs.age"
    ;;
  age-keygen)
    attr="third_party.nixpkgs.age"
    ;;
  gerrit)
    attr="tools.gerrit-cli"
    ;;
  gerrit-update)
    attr="tools.gerrit-update"
    ;;
  hash-password)
    attr="tools.hash-password"
    ;;
  mg)
    attr="tools.magrathea"
    ;;
  nint)
    attr="nix.nint"
    ;;
  rebuild-system)
    attr="ops.nixos.rebuild-system"
    ;;
  rink)
    attr="third_party.nixpkgs.rink"
    ;;
  stern)
    attr="third_party.nixpkgs.stern"
    ;;
  depotfmt)
    attr="tools.depotfmt"
    ;;
  tf-glesys)
    TARGET_TOOL="terraform"
    attr="ops.glesys.terraform"
    ;;
  tf-keycloak)
    TARGET_TOOL="terraform"
    attr="ops.keycloak.terraform"
    ;;
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    exit 1
    ;;
esac

result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")
PATH="${result}/bin:$PATH"

exec "${TARGET_TOOL}" "${@}"
