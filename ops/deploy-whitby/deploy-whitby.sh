#!/usr/bin/env bash
set -Ceuo pipefail

HTML_ROOT="${HTML_ROOT:-/var/html/deploys.tvl.fyi}"
URL_BASE="${URL_BASE:-https://deploys.tvl.fyi/diff}"
IRCCAT_PORT="${IRCCAT_PORT:-4722}"

drv_hash() {
    basename "$1" | sed 's/-.*//'
}

new_rev="$1"

if [ -z "$new_rev" ]; then
    >&2 echo "Usage: $0 <new_rev>"
    exit 1
fi

if [ -d "/tmp/deploy.worktree" ]; then
    >&2 echo "/tmp/deploy.worktree exists - exiting in case another deploy is currently running"
    exit 1
fi

worktree_dir=/tmp/worktree_dir

cleanup() {
    rm -rf "$worktree_dir"
}
trap cleanup EXIT

git clone https://cl.tvl.fyi/depot "$worktree_dir" --reference /depot
git -C "$worktree_dir" checkout "$new_rev"

current=$(nix show-derivation /run/current-system | jq -r 'keys | .[0]')
new=$(nix-instantiate -A ops.nixos.whitbySystem "$worktree_dir")

diff_filename="$(drv_hash "$current")..$(drv_hash "$new")"
nix-diff "$current" "$new" --color always \
    | ansi2html \
    >| "$HTML_ROOT/diff/$diff_filename"

echo "#tvl whitby is being deployed! system diff: $URL_BASE/$diff_filename" \
    | nc -w 5 -N localhost "$IRCCAT_PORT"

# TODO(grfn): Actually do the deploy
