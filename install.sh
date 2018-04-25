#!/usr/bin/env zsh

source "${DOTFILES}/bins/setup"
source "${DOTFILES}/configs/setup"
ln -s "${DOTFILES}/emacs.d" "${HOME}/.emacs.d"
