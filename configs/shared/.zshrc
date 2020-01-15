# Necessary to add this line and keep it at the top of my ~/.zshrc to ensure
# that Tramp works as expected. This was taken from here:
# https://www.emacswiki.org/emacs/TrampMode
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

source ~/antigen.zsh

# Helper functions
# TODO: Find a way to share these functions across zsh files.
defensively_source() {
  # Checks if the file exists before sourcing it
  if [ -f $1 ]; then
    source $1
  else
    "Cannot source: $1. Aborting..."
  fi
}

command_exists() {
  # Predicate to check if the command exists
  command -v $1 >/dev/null
}

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
  wpcarro2)
    antigen theme refined;;
  # acer NixOS laptop
  acer-manjaro)
    antigen theme frisk;;
esac

# Leave this last
antigen apply

# Configure fzf
if command_exists fzf-share; then
  source "$(fzf-share)/key-bindings.zsh"

  fzf-locate-widget() {
    # Press M-i to search entire locate database with fzf.
    local selected
    if selected=$(locate / | fzf); then
      LBUFFER+=$selected
    fi
    zle redisplay
  }
  zle -N fzf-locate-widget
  bindkey '\ei' fzf-locate-widget

else
  defensively_source "/usr/share/fzf/key-bindings.zsh"
fi

# Configure fasd
eval "$(fasd --init auto)"

# the above line slows tab-completion down dramatically because it attemtps to
# autocomplete for the 600k+ users found in `compgen -u`. Below is a fix which
# also restores the function of `cd ~<tab>` to display only ZSH Named
# Directories.
zstyle ':completion:*' users root $USER

# Avoiding the defensive source because I want to ensure these files are
# available.
source "$DOTFILES/configs/shared/variables.zsh"
source "$DOTFILES/configs/shared/aliases.zsh"
source "$DOTFILES/configs/shared/functions.zsh"
source "$DOTFILES/configs/shared/zle.zsh"

preexec() {
  # `preexec` runs before every command is run.
  update_x11_forwarding
}

defensively_source ~/.rvm/scripts/rvm         # Ruby
defensively_source ~/.opam/opam-init/init.zsh # OCaml
defensively_source ~/.ghcup/env               # Haskell
defensively_source /etc/bash_completion.d/g4d # Google's g4d
