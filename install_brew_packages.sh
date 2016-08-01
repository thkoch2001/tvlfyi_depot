#!/usr/bin/env bash

pushd "$HOME/pc_settings"

# install brew dependencies
cat ./brew_packages.txt | xargs brew install

popd

