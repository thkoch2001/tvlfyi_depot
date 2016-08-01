#!/usr/bin/env bash

# This script points the .vim directory to a USB to increase portability
# it will toggle between modes

if [ -L "$HOME/.vim" ]; then
  echo "Already pointing to USB. Toggling back..."

  # remove the symlink and .vimrc
  rm "$HOME/.vim"

  # remove the USB's version of the .vimrc and use the backed-up copy
  rm "$HOME/.vimrc"
  mv "$HOME/.vimrc.bak" "$HOME/.vimrc"

  # rename the .vim.bak directory
  mv "$HOME/.vim.bak" "$HOME/.vim"

  echo ".vim now points to $HOME/.vim"
else
  echo "Not pointing to USB. Getting to work..."

  # rename the current .vim directory and .vimrc
  mv "$HOME/.vim" "$HOME/.vim.bak"
  mv "$HOME/.vimrc" "$HOME/.vimrc.bak"

  # point the $HOME/.vim name to the USB for source routing
  # use the USB drive's copy of .vimrc
  ln -s /Volumes/Untitled\ 1/.vim "$HOME/.vim"
  cp /Volumes/Untitled\ 1/.vimrc "$HOME/"

  echo ".vim now points to /Volumes/Untitled\ 1/.vim"
fi

echo "Done."

