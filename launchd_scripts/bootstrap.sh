#!/usr/bin/env bash


# Unload scripts in case there have been changes since it was last loaded.
echo -n "Unloading personal *.plist files... " &&
launchctl unload ~/Library/LaunchAgents/watch_volumes.plist &&
echo "Done." || echo "Error."


# Remove *.plist symlinks created last time.
echo -n "Removing *.plist symlinks... " &&
rm ~/Library/LaunchAgents/watch_volumes.plist &&
echo "Done." || echo "Error."


# Process the *.tpl files, replacing global identifiers with the values
# from vars.json.
echo -n "Processing *.tpl files... " &&
. ./process_files.sh &&
echo "Done." || echo "Error."


# Recreate those symlinks.
echo -n "Recreating *.plist symlinks to ~/Library/LaunchAgents ... " &&
ln -s ~/pc_settings/launchd_scripts/watch_volumes.plist \
  ~/Library/LaunchAgents/watch_volumes.plist &&
echo "Done." || echo "Error."


# Reload scripts in case there have been changes since it was last loaded.
echo -n "Reloading personal *.plist files... " &&
launchctl load ~/Library/LaunchAgents/watch_volumes.plist &&
echo "Done." || echo "Error."

