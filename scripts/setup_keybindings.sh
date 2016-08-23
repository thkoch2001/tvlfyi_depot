#!/usr/bin/env bash


# This file is run after history_functions.sh have been sourced.
# It converts the defined functions into zsh widgets that are
# thereafter bound to keys for expedience.


zle -N wh_two_back_widget wh_two_back &&
bindkey '^@' wh_two_back_widget

