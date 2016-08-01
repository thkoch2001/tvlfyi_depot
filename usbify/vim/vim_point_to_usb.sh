#!/usr/bin/env bash

# This script toggles between local vim and a version that can be stored on an 
# external device like a USB.

if [ -L "$HOME/.vim" ]; then
  echo "Pointing to USB. Toggling back to local machine..."

  # remove the symlink and .vimrc
  rm "$HOME/.vim"

  # remove the USB's version of the .vimrc and use the backed-up copy
  rm "$HOME/.vimrc"
  mv "$HOME/.vimrc.bak" "$HOME/.vimrc"

  # rename the .vim.bak directory
  mv "$HOME/.vim.bak" "$HOME/.vim"

  echo ".vim now points to $HOME/.vim"
else
  echo "Pointing to local machine. Toggling to USB..."

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

