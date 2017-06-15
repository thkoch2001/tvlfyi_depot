#!/usr/bin/env zsh


if [ -n "$INSIDE_EMACS" ]; then
    export PAGER="create-shell-pager.sh"
else
    export PAGER="less"
fi


if [ -n "$INSIDE_EMACS" ]; then
    export EDITOR="edit-file-in-emacs.sh"
else
    export EDITOR=$(which vim)
fi


vim () {
    if [ -n "$INSIDE_EMACS" ]; then
        emacsclient -e "(find-file-other-window \"$1\")"
    else
        eval "$EDITOR \"$1\""
    fi
}


man () {
    if [ -n "$INSIDE_EMACS" ]; then
        emacsclient -e  "(man \"$1\")"
    else
        command man "$1"
    fi
}
