#!/usr/bin/env zsh

# This file contains tooling that make terminal usage from within Emacs more enjoyable. Some of
# these are conditionally defined functions that monkey patch CLI utils like vim and man and
# dispatch to native Emacs utils that should function as analogous alternatives.

# While most of these conditional definitions could fall inside of one larger, if "${INSIDE_EMACS}"
# block, to increase portability, smaller, redundant blocks are preferred.


# Have zsh export variables for Emacs to track the cwd
if [ -n "${INSIDE_EMACS}" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi


# Custom emacs pager support
if [ -n "${INSIDE_EMACS}" ]; then
    export PAGER="create-shell-pager.sh"
fi


# For git primarily. Edit commit messages, etc from ansi-term in Emacs
if [ -n "${INSIDE_EMACS}" ]; then
    export EDITOR="edit-file-in-emacs.sh"
fi


# Muscle-memory dies hard. Calls to vim from an Emacs terminal attempt to open vim from inside of
# Emacs. This is a really bad UX, and hard to exit from. Instead of retraining muscle-memory,
# dispatch to Emacs' file editing when vim is called from an Emacs terminal.
if [ -n "${INSIDE_EMACS}" ]; then
    function vim () {
        emacsclient -e "(find-file-other-window \"$1\")"
    }
fi


# Prefer Emac's built-in man util when called from an Emacs terminal
if [ -n "${INSIDE_EMACS}" ]; then
    function man () {
        emacsclient -e  "(man \"$1\")"
    }
fi
