#!/usr/bin/env bash

# This script is used to ensure the USB has the latest code from the repository.

# Update the following values to reflect the locations of each directory on your
# particular machine.
path_to_local_repo="$HOME/pc_settings" # path to git repo
path_to_ext_device="/Volumes/usb_vim" # path to USB device


if [ ! -d "$path_to_ext_device" ]; then
  echo "No external device found at ${path_to_ext_device}." && \
  echo "Make sure the values input within update.sh are correct." && \
  echo "path_to_ext_device: $path_to_ext_device" && \
  echo "Exiting." && return 1
fi

if [ ! -d "$path_to_local_repo" ]; then
  echo "No repository found at ${path_to_local_repo}." && \
  echo "Make sure the values input within update.sh are correct." && \
  echo "path_to_local_repo: $path_to_local_repo" && \
  echo "Exiting." && return 1
fi


# Update the local copy of the repo.
echo -n "Updating pc_settings..." && \
  pushd "$path_to_local_repo" >/dev/null && \
  git pull origin master >/dev/null

  if [ ! "$?" -eq 0 ]; then
    echo "Error: git pull error. Exiting." && \
    return 1
  else
    echo "Done."
  fi


# Copy vim directory to USB device.
echo -n "Copying files to external device..." && \
  pushd "$path_to_ext_device" >/dev/null && \
  rm -rf "${path_to_ext_device}/vim" &>/dev/null && \
  cp -r "${path_to_local_repo}/usbify/vim" "${path_to_ext_device}/vim" \
      &>/dev/null && \

  if [ ! "$?" -eq 0 ]; then
    echo "Error: rm or cp error. Exiting." && \
    return 1
  else
    echo "Done."
  fi


# restore the dirs to its state before running this script
popd >/dev/null && popd >/dev/null && echo "Complete." && return 0
