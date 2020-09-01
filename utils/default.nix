args@{ pkgs, ... }:

# This top-level module exposes all of my utility functions for Nix. It should
# be used like:
# ```nix
# inherit (briefcase.utils) fs;
# ```

let
  builder = import ./builder.nix args;
  fs      = import ./fs.nix args;
in {
  inherit builder fs;
}
