#!/usr/bin/env bash


HISTFILE=~/.zsh_history


function wh_two_back {
  command=$(history | tail -n 2 | head -n 1 | cut -c 8-)
  echo -n $command
}


function wh_three_back {
  command=$(history | tail -n 3 | head -n 1 | cut -c 8-)
  echo -n "$command"
}


function wh_four_back {
  command=$(history | tail -n 4 | head -n 1 | cut -c 8-)
  echo -n "$command"
}


function wh_five_back {
  command=$(history | tail -n 5 | head -n 1 | cut -c 8-)
  echo -n "$command"
}

