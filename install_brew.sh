#!/usr/bin/env bash

echo "Installing Homebrew..."
$(which ruby) -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/uninstall)"
$(which ruby) -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
echo "Homebrew installed."
echo ""

pushd "$HOME/pc_settings"

# install brew dependencies
echo "Installing Homebrew packages..."
cat ./brew_packages.txt | xargs brew install
echo "Homebrew packages installed."
echo ""

echo "Homebrew installed complete!"
echo ""

popd

