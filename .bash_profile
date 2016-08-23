export PATH=$HOME/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH


echo "Welcome back, $USER"

# use vi bindings for terminal input
set -o vi

# aliases
source $HOME/pc_settings/.w_aliases.sh

# functions
source $HOME/pc_settings/.w_functions.sh

# syntax highlighting for CLI; if not installed, run the following command
# brew install zsh-syntax-highlighting

