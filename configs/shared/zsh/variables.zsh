export TERMINAL=urxvtc
export EDITOR=emacsclient

BROWSER=google-chrome
NIXIFY="$HOME/programming/nixify"
DATE_FMT=%b-%d-%Y_%T # my preferred date formatting string used for generated filename

# ZSH's static named directories
hash -d pro="$HOME/programming"
hash -d dot="$HOME/programming/dotfiles"
hash -d citc="/google/src/cloud/$USER"
hash -d doc="$HOME/Documents"
hash -d d="$HOME/Downloads"

# commonly used config files
v="$HOME/.config/nvim/init.vim"
e="$HOME/.emacs.d/init.el"
z="$HOME/.zshrc"
a="$HOME/aliases.zsh"
f="$HOME/functions.zsh"
l="$HOME/variables.zsh" # v is taken by vim
x="$HOME/.Xresources"
i="$HOME/.config/i3/config"
