#!/usr/bin/env zsh


if [ -n "$INSIDE_EMACS" ]; then
    export PAGER="create-shell-pager.sh"
else
    export PAGER="less"
fi


if [ -n "$INSIDE_EMACS" ]; then
    export EDITOR="emacsclient"
else
    export EDITOR=$(which vim)
fi


man () {
    if [ -n "$INSIDE_EMACS" ]; then
        emacsclient -e  "(man \"$1\")"
    else
        command man "$1"
    fi
}
