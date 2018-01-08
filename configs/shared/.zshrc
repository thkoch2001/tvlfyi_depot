export ZSH=${HOME}/.oh-my-zsh

ZSH_THEME="refined"
plugins=(tmux zsh-autosuggestions git git-extras github gitfast)
source $ZSH/oh-my-zsh.sh
source ~/.zsh_profile

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/wpcarro/Downloads/google-cloud-sdk/path.zsh.inc' ]; then source '/Users/wpcarro/Downloads/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/wpcarro/Downloads/google-cloud-sdk/completion.zsh.inc' ]; then source '/Users/wpcarro/Downloads/google-cloud-sdk/completion.zsh.inc'; fi
