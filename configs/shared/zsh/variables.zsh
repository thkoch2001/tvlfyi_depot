export GPG_TTY=$(tty) # "It is important that this environment variable always reflects the output of the tty command". Source: https://gnupg.org/documentation/manuals/gnupg-devel/Invoking-GPG_002dAGENT.html

date_fmt=%b-%d-%Y_%T # my preferred date formatting string used for generated filename

# ZSH's static named directories
hash -d pro=~/programming
hash -d dot="$DOTFILES"
hash -d citc=/google/src/cloud/$USER
hash -d doc=~/Documents
hash -d d=~/Downloads
hash -d ss=~/Pictures/screenshots
hash -d fonts="$DOTFILES/configs/shared/misc/.local/share/fonts"
hash -d sounds="$DOTFILES/configs/shared/misc/.local/share/sounds"
hash -d wallpaper="$DOTFILES/configs/shared/misc/.local/share/wallpaper"
hash -d org="$ORG_DIRECTORY"
hash -d x20=/google/data/rw/users/wp/wpcarro # remember: to access x20, you need to run `prodaccess`
# named directories for commonly used projects
hash -d korvus_framework=./java/com/google/corp/sales
hash -d korvus_services=./corp/sales/casesautomation
hash -d escalations=./corp/gtech/pto/tda/beacons_extension
hash -d ultra=./ads/doubleclick/systems/crm
hash -d incentives_fe=./experimental/adservices/jarvis/jarvis_extension/jarvis_staging/js
hash -d incentives_be=./experimental/adservices/tesseract/handlers/incentives

# 8-bit colors
black='\u001b[30m'
red='\u001b[31m'
green='\u001b[32m'
yellow='\u001b[33m'
blue='\u001b[34m'
magenta='\u001b[35m'
cyan='\u001b[36m'
white='\u001b[37m'
bright_black='\u001b[30;1m'
bright_red='\u001b[31;1m'
bright_green='\u001b[32;1m'
bright_yellow='\u001b[33;1m'
bright_blue='\u001b[34;1m'
bright_magenta='\u001b[35;1m'
bright_cyan='\u001b[36;1m'
bright_white='\u001b[37;1m'

# some of my favorite emojis as unicode
facepalm='\U1F926'
eyeroll='\U1F644'
sheesh='\U1F62C'
see_no_evil='\U1F648'
blush='\U1F60A'
wink='\U1F609'
laugh='\U1F923'

# commonly used config files
v="$HOME/.config/nvim/init.vim"
e="$HOME/.emacs.d/init.el"
z="$HOME/.zshrc"
a="$HOME/aliases.zsh"
f="$HOME/functions.zsh"
l="$HOME/variables.zsh" # v is taken by vim
x="$HOME/.Xresources"
i="$HOME/.config/i3/config.shared"
