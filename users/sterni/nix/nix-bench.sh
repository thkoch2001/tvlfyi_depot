#!/usr/bin/env nix-shell
#!nix-shell --pure
#!nix-shell -p nix
#!nix-shell -p time
#!nix-shell -p dash
#!nix-shell -i dash

test "$#" -ge 1 || exit 100

RANGE="10 20 30 40 50 60 70 80 90 100 200 300 400 500 600 700 800 900 1000 2000 3000 4000 5000 10000 25000 50000 100000"
F="$1"

for s in $RANGE; do
  printf '%s ' "$s" >&2
  time -f"%e" nix-instantiate --eval --strict -E "($F) $s"
done
