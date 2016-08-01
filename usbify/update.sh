#!/usr/bin/env bash

# This script is used to ensure the USB has the latest code from the repository.

# Update the following values to reflect the locations of each directory on your
# particular machine.
path_to_local_repo="$HOME/pc_settings" # path to git repo
path_to_ext_device="/Volumes/usb_vim/" # path to USB device

if [ ! -d "$path_to_ext_device" ]; then
  echo "No external device found at ${path_to_ext_device}."
  echo "Make sure the values input within update.sh are correct."
  echo "path_to_ext_device: $path_to_ext_device"
  echo "Exiting."
  return 1
fi

if [ ! -d "$path_to_local_repo" ]; then
  echo "No repository found at ${path_to_local_repo}."
  echo "Make sure the values input within update.sh are correct."
  echo "path_to_local_repo: $path_to_local_repo"
  echo "Exiting."
  return 1
fi

pushd "$path_to_ext_device" >/dev/null

# Update the local copy of the repo.
echo "Updating pc_settings..."
pushd "$path_to_local_repo" >/dev/null
git pull origin master
echo ""

echo "Copying files to external device..."
popd # $(pwd) -eq $path_to_ext_device
# copy the vim contents from $HOME/pc_settings into $path_to_ext_device
rm -rf ./vim
cp -r "${path_to_local_repo}/usbify/vim" .
echo ""

popd # restore the dirs to its state before running this script

echo "Done."

