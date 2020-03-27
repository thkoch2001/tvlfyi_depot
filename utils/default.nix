{ pkgs, ... }:

# Using this as a library to define some common utility functions that I often
# reach for.
{
  # A unary function that returns its argument.
  identity = x: x;

  # Create a derivation that creates an executable shell script named `as` that
  # calls the program located at `path`, forwarding all of the arguments.
  wrapNonNixProgram = { path, as }: pkgs.writeShellScriptBin as ''
    exec ${path} "$@"
  '';

  # Expose the buildInputs from a Nix shell to an Emacs buffer. Intended to be
  # called from dir-locals.nix files.
  nixBufferFromShell = path: let
    shell = import path;
  in pkgs.nixBufferBuilders.withPackages shell.buildInputs;
}
