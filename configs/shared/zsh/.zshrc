export META_DIR="${HOME}/urbint/meta"
export DOTFILES="${HOME}/dotfiles"
export ZSH="${HOME}/.oh-my-zsh"
export PATH="${PATH}:${HOME}/.local/bin"
export NIX_PKGS="${HOME}/programming/nixpkgs/pkgs"

# Put this here temporarily until we have a better place for it.
export FPP_EDITOR='emacsclient -n'

ZSH_THEME=refined
plugins=(zsh-autosuggestions git git-extras github)

source "${ZSH}/oh-my-zsh.sh"
source "${META_DIR}/urbint_101/scripts/setup"
