#!/usr/bin/env bash

path_to_ext_device="/Volumes/usb_vim"

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

  # back-up local machine's .vim folder
  mv "$HOME/.vim" "$HOME/.vim.bak"

  # back-up the local machine's .vimrc
  if [ -f "HOME/.vimrc" ]; then
    mv "$HOME/.vimrc" "$HOME/.vimrc.bak"
  fi


  # point the $HOME/.vim name to the USB for source routing
  # use the USB drive's copy of .vimrc
  ln -s "${path_to_ext_device}/.vim" "$HOME/.vim"
  ln -s "${path_to_ext_device}/.vimrc" "$HOME/.vimrc"

  echo ".vim now points to ${path_to_ext_device}/.vim"
fi

echo "Done."

