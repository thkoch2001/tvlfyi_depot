export PATH=$HOME/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:$PATH


echo "Welcome back, $USER"

# use vi bindings for terminal input
set -o vi

# aliases
source $HOME/pc_settings/.w_aliases.sh

# functions
source $HOME/pc_settings/.w_functions.sh

# run cmatrix command for 3 seconds before exiting
#. $HOME/pc_settings/.matrix_intro.sh

# syntax highlighting for CLI; if not installed, run the following command
# brew install zsh-syntax-highlighting
source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

