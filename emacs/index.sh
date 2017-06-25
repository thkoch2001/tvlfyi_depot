#!/usr/bin/env zsh


# Have zsh export variables for Emacs to track the cwd
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi


# Custom emacs pager support
if [ -n "$INSIDE_EMACS" ]; then
    export PAGER="create-shell-pager.sh"
else
    export PAGER="less"
fi


# Edit commit messages, etc from ansi-term in emacs
if [ -n "$INSIDE_EMACS" ]; then
    export EDITOR="edit-file-in-emacs.sh"
else
    export EDITOR="command vim"
fi


# Calls to vim from within ansi-term trigger emacs find-file
vim () {
    if [ -n "$INSIDE_EMACS" ]; then
        emacsclient -e "(find-file-other-window \"$1\")"
    else
        command vim "$1"
    fi
}


# Calls to man from within ansi-term trigger emacs man
man () {
    if [ -n "$INSIDE_EMACS" ]; then
        emacsclient -e  "(man \"$1\")"
    else
        command man "$1"
    fi
}
