#/usr/bin/env zsh

file=$(mktemp -t "$USER-"XXXXXXXX.emacs-pager) || exit 127
trap 'rm -f "$file"' EXIT
trap 'exit 255' HUP INT QUIT TERM
cat "$@" >"$file"
emacsclient -e "(wc/edit-file-in-emacs \"$file\")"
