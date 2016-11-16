#!/usr/bin/env bash

path_to_ext_device="/Volumes/usb_vim"


# ensure there is an external device connected and that the path to it is 
# accurate.
if [ ! -d "$path_to_ext_device" ]; then
  echo "No external device found at: $path_to_ext_device"
  echo "Ensure that the value set for path_to_ext_device is correct."
  echo "path_to_ext_device: $path_to_ext_device"
  echo "Exiting."
  return 1
fi



# This script toggles between local vim and a version that can be stored on an 
# external device like a USB.

# USB --> local machine
if [ -L "$HOME/.vim" ] && [ -L "$HOME/.vimrc" ]; then
  echo "Pointing to USB. Toggling back to local machine..."

  # remove the symlinks
  rm "$HOME/.vim"
  rm "$HOME/.vimrc"

  # restore back-ups as active files
  [ -d "$HOME/.vim.bak" ] && mv "$HOME/.vim.bak" "$HOME/.vim"
  [ -f "$HOME/.vimrc.bak" ] && mv "$HOME/.vimrc.bak" "$HOME/.vimrc"

  echo ".vim now points to $HOME/.vim"
  echo ".vimrc now points to $HOME/.vimrc"

# local machine --> USB
else
  echo "Pointing to local machine. Toggling to USB..."

  # back-up local machine's files
  [ -d "$HOME/.vim" ] && mv "$HOME/.vim" "$HOME/.vim.bak"
  [ -f "$HOME/.vimrc" ] && mv "$HOME/.vimrc" "$HOME/.vimrc.bak"

  # symlink .vim and .vimrc to external device
  ln -s "${path_to_ext_device}/vim/.vim" "$HOME/.vim"
  ln -s "${path_to_ext_device}/vim/.vimrc" "$HOME/.vimrc"

  echo ".vim now points to ${path_to_ext_device}/vim/.vim"
fi

echo "Done."

