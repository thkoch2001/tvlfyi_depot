source ~/antigen.zsh

# Load the oh-my-zsh library
antigen use oh-my-zsh

# Bundles from robbyrussell's oh-my-zsh repo.
antigen bundle git
antigen bundle alias-tips      # friendly reminders to prefer an alias if exists
# antigen bundle common-aliases  # be careful with the load order here. Can easily eclipse aliases undesirably
antigen bundle extract         # extracts archives polymorphically
antigen bundle zsh-completions # extracts archives polymorphically

# Syntax highlighting
antigen bundle zsh-users/zsh-syntax-highlighting

# Theming
antigen theme robbyrussell

# Leave this last
antigen apply

# Personal Configuration

# Set environment variables for Nix
source /usr/local/google/home/wpcarro/.nix-profile/etc/profile.d/nix.sh

# Configure fzf
source "$(fzf-share)/key-bindings.zsh"

# Configure fasd
eval "$(fasd --init auto)"

# use full path instead of $DOTFILES, since DOTFILES is set herein
DOTFILES="$HOME/programming/dotfiles"
source "$DOTFILES/configs/shared/zsh/variables.zsh"
source "$DOTFILES/configs/shared/zsh/aliases.zsh"
source "$DOTFILES/configs/shared/zsh/functions.zsh"
source "$DOTFILES/configs/shared/zsh/dumping_grounds.zsh"
