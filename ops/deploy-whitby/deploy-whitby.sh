#!/usr/bin/env bash
set -eu

HTML_ROOT="${HTML_ROOT:-/srv/www/deploy.tvl.fyi}"
URL_BASE="${URL_BASE:-https://deploy.tvl.fyi/diff}"
IRCCAT_PORT="${IRCCAT_PORT:-4722}"

depot_git() {
    git -C /depot "$@"
}

drv_hash() {
    basename "$1" | sed 's/-.*//'
}

new_rev="$1"

if [ -z "$new_rev" ]; then
    echo "Usage: $0 <new_rev>"
    exit 1
fi

if [ -d "/tmp/deploy.worktree" ]; then
    echo "/tmp/deploy.worktree exists - exiting in case another deploy is currently running"
    exit 1
fi

cleanup() {
    rm -rf /tmp/deploy.worktree
    depot_git worktree prune
}
trap cleanup EXIT

depot_git worktree add /tmp/deploy.worktree "$new_rev"

current=$(nix show-derivation /run/current-system | jq -r 'keys | .[0]')
new=$(nix-instantiate -A ops.nixos.whitbySystem /tmp/deploy.worktree)

diff_filename="$(drv_hash "$current")..$(drv_hash "$new")"
nix-diff "$current" "$new" --color always \
    | ansi2html \
    > "$HTML_ROOT/diff/$diff_filename"

echo "#tvl Whitby is being deployed! System diff: $URL_BASE/$diff_filename" \
    | nc localhost "$IRCCAT_PORT"

# TODO(grfn): Actually do the deploy
