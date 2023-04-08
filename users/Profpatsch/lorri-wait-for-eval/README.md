# lorri-wait-for-eval

A helper script for [lorri](https://github.com/nix-community/lorri), which wraps a command and executes it once lorri is finished evaluating the current `shell.nix`, and uses the new environment.

This is useful when you need the new shell environment to be in scope of the command, but don’t want to waste time waiting for it to finish.

This should really be a feature of lorri, but I couldn’t be assed to touch rust :P
