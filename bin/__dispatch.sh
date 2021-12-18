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
    attr="third_party.nixpkgs-age"
    ;;
  age-keygen)
    attr="third_party.nixpkgs.age"
    ;;
  depot-build)
    attr="tools.depot-build"
    ;;
  depot-nixpkgs-update)
    attr="tools.depot-nixpkgs-update"
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
  kontemplate)
    attr="ops.kontemplate"
    ;;
  meson)
    attr="third_party.nixpkgs.meson"
    ;;
  mg)
    attr="tools.magrathea"
    ;;
  ninja)
    attr="third_party.nixpkgs.ninja"
    ;;
  nint)
    attr="nix.nint"
    ;;
  perf-flamegraph)
    attr="tools.perf-flamegraph"
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
  *)
    echo "The tool '${TARGET_TOOL}' is currently not installed in this repository."
    exit 1
    ;;
esac

result=$(nix-build --no-out-link --attr "${attr}" "${REPO_ROOT}")
PATH="${result}/bin:$PATH"

exec "${TARGET_TOOL}" "${@}"
