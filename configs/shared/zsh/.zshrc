source ~/.profile

source ~/antigen.zsh

# Load the oh-my-zsh library
antigen use oh-my-zsh

# Bundles from robbyrussell's oh-my-zsh repo.
antigen bundle git
antigen bundle extract         # extracts archives polymorphically
antigen bundle zsh-completions # extracts archives polymorphically

# Syntax highlighting
antigen bundle zsh-users/zsh-syntax-highlighting

# Theming
case $(hostname) in
  # desktop
  wpcarro.lon.corp.google.com)
    antigen theme frisk;;
  # cloudtop
  wpcarro.c.googlers.com)
    antigen theme cloud;;
  # laptop
  wpcarro)
    antigen theme refined;;
esac

# Leave this last
antigen apply

# Configure fzf
source "$(fzf-share)/key-bindings.zsh"

# Configure fasd
eval "$(fasd --init auto)"

# Configure g4 with zsh
if [ -f /etc/bash_completion.d/g4d ]; then
  source /etc/bash_completion.d/g4d
fi
# the above line slows tab-completion down dramatically because it attemtps to
# autocomplete for the 600k+ users found in `compgen -u`. Below is a fix which
# also restores the function of `cd ~<tab>` to display only ZSH Named
# Directories.
zstyle ':completion:*' users root $USER

source "$DOTFILES/configs/shared/zsh/variables.zsh"
source "$DOTFILES/configs/shared/zsh/aliases.zsh"
source "$DOTFILES/configs/shared/zsh/functions.zsh"
source "$DOTFILES/configs/shared/zsh/zle.zsh"
