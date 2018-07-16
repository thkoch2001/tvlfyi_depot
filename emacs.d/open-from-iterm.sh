#!/bin/bash

# To set this up, open iTerm2 -> Preferences -> Profiles -> Advanced
# In the "Semantic History" section, choose "Always run command..." from the
# dropdown and set the input text to:
# `~/dotfiles/emacs.d/open-from-iterm.sh \1`

# Alias applications since $PATH is unavailable
emacs=/usr/local/bin/emacsclient
grep=/usr/local/bin/ggrep
tmux=/usr/local/bin/tmux
realpath=/Users/wpcarro/.nix-profile/bin/realpath

e() {
  # Useful debugger when paired with `tail -f /tmp/debug.log`
  echo "$@" >>/tmp/debug.log
}

# Need to query Tmux since iTerm's \5 variable doesn't work as expected with
# Tmux.
pwd=$($tmux display -pF '#{pane_current_path}')
cd "$pwd" || exit
path=$($realpath "$1")

# This is a bit of a hack, but we cannot rely on iTerm to regex our paths
file=$($grep -P -o '^[^:]+' <<<"$path")
number=$($grep -P -o '(?<=:)[0-9]+(?=:[0-9]+:$)' <<<"$path")

# Debugging
e "file:   $file"
e "number: $number"

if ! [ -z "$number" ]; then
  $emacs -n -e "(find-file \"$file\")" "(goto-line $number)"
else
  $emacs -n -e "(find-file \"$file\")"
fi
