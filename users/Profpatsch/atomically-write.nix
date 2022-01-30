{ depot, pkgs, ... }:
# Atomically write a file (just `>` redirection in bash
# empties a file even if the command crashes).
#
# Maybe there is an existing tool for that?
# But it’s easy enough to implement.
#
# Example:
#   atomically-write
#     ./to
#     echo "foo"
#
# will atomically write the string "foo" into ./to
let
  atomically-write = pkgs.writers.writeDash "atomically-write" ''
    set -e
    to=$1
    shift
    # assumes that the tempfile is on the same file system, (or in memory)
    # for the `mv` at the end to be more-or-less atomic.
    tmp=$(${pkgs.coreutils}/bin/mktemp -d)
    trap 'rm -r "$tmp"' EXIT
    "$@" \
      > "$tmp/out"
    mv "$tmp/out" "$to"
  '';

in
atomically-write
