export ZSH=/Users/wpcarro/.oh-my-zsh

ZSH_THEME="spaceship"
plugins=(tmux zsh-autosuggestions git git-extras github gitfast)
source $ZSH/oh-my-zsh.sh
source ~/.zsh_profile

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).
