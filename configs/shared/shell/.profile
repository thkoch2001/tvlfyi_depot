# Some programs read from ~/.profile for values. It's best to set environment
# variables here instead of in ~/.zshrc or similar files, which are sourced
# everytime a new shell is created. The ~/.profile, on the other hand, is
# typically sourced only once at login.

PATH="$PATH:$HOME/bin"

# Application preferences
export BROWSER=google-chrome
export TERMINAL=st
export EDITOR=emacsclient
export ALTERNATE_EDITOR=nvim

# Application configuration
export FZF_DEFAULT_COMMAND='fd --hidden --follow --exclude ".git"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

export DOTFILES="$HOME/Dropbox/dotfiles"
export ORG_DIRECTORY="$HOME/Dropbox/org"

# Set environment variables for Nix
source ~/.nix-profile/etc/profile.d/nix.sh
