export META_DIR="${HOME}/urbint/meta"
export ZSH="${HOME}/.oh-my-zsh"

ZSH_THEME=refined
plugins=(zsh-autosuggestions git git-extras github)

source "${ZSH}/oh-my-zsh.sh"
source "${META_DIR}/urbint_101/scripts/setup"
