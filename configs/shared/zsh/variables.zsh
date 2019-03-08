export TERMINAL=urxvtc
export EDITOR=emacsclient
export ALTERNATE_EDITOR=nvim

BROWSER=google-chrome
NIXIFY="$HOME/programming/nixify"
DATE_FMT=%b-%d-%Y_%T # my preferred date formatting string used for generated filename

# ZSH's static named directories
hash -d pro=~/programming
hash -d dot=~/programming/dotfiles
hash -d citc=/google/src/cloud/$USER
hash -d doc=~/Documents
hash -d d=~/Downloads
hash -d ss=~/Pictures/screenshots
hash -d org=~/Documents/org
# named directories for commonly used projects
hash -d korvus_framework=./java/com/google/corp/sales
hash -d korvus_services=./corp/sales/casesautomation
hash -d escalations=./corp/gtech/pto/tda/beacons_extension
hash -d ultra=./ads/doubleclick/systems/crm
hash -d incentives_fe=./experimental/adservices/jarvis/jarvis_extension/jarvis_staging/js
hash -d incentives_be=./experimental/adservices/tesseract/handlers/incentives

# commonly used config files
v="$HOME/.config/nvim/init.vim"
e="$HOME/.emacs.d/init.el"
z="$HOME/.zshrc"
a="$HOME/aliases.zsh"
f="$HOME/functions.zsh"
l="$HOME/variables.zsh" # v is taken by vim
x="$HOME/.Xresources"
i="$HOME/.config/i3/config"
