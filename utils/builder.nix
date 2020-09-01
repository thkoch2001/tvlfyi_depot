{ pkgs, ... }:

let
  inherit (pkgs) writeShellScriptBin;
in {
  # Create a derivation that creates an executable shell script named `as` that
  # calls the program located at `path`, forwarding all of the arguments.
  wrapNonNixProgram = { path, as }: writeShellScriptBin as ''
    exec ${path} "$@"
  '';
}
