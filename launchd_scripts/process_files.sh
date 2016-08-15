#!/usr/bin/env bash

# This script processes certain files and replaces
# {{<IDENTIFIER>}} with the entries in vars.json

output_path="./watch_volumes.plist"
template_file="watch_volumes.plist.tpl"
usb_drive_path=$(jq < ./vars.json '.USB_DRIVE_PATH' | \
  sed 's/\//\\\//g' | sed 's/"//g')

cat "$template_file" | perl -p -e 's/(\{\{[^}]+\}\})/'$usb_drive_path'/g' \
  >"$output_path"

echo "Done."

