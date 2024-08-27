# Helper functions for extending Eagle Mode with useful stuff.
#
# Eagle Mode's customisation usually expects people to copy the entire
# configuration into their user folder, which we can automate fairly easily
# using Nix, letting users choose whether to keep upstream config or not.
{ depot, lib, pkgs, ... }:

let
  mkDesc = d: lib.concatMapStringsSep "\n"
    (x: "# Descr =${x}")
    (builtins.filter (s: s != "") (lib.splitString "\n" d));
in
rec {
  # mkCommand creates an Eagle Mode command for the file browser.
  #
  # Commands are basically little Perl scripts with a command standard library
  # available. They receive the user's selected target from Eagle Mode.
  mkCommand =
    {
      # Name of the command.
      name
    , # User-facing description, displayed in Eagle Mode UI. Can be multi-line.
      description
    , # Verbatim Perl code of the command. Command library is already available.
      code
    , # Caption for the UI button (defaults to name).
      caption ? name
    , icon ? "terminal.tga"
    , # TODO: what's a good default?
      hotkey ? ""
    , order ? 1.0
    }: pkgs.writeTextDir "emFileMan/Commands/${name}.pl" (''
      #!${pkgs.perl}/bin/perl
      #[[BEGIN PROPERTIES]]
      # Type = Command
      # Interpreter = perl
      # DefaultFor = directory
      # Caption = ${caption}
      # Order = ${toString order}
      # Icon = ${icon}
    ''
    + (lib.optionalString (description != "") "${mkDesc description}\n")
    + (lib.optionalString (hotkey != "") "# Hotkey = ${hotkey}\n")
    + ''
      #[[END PROPERTIES]]

      use strict;
      use warnings;
      BEGIN { require "$ENV{'EM_DIR'}/res/emFileMan/scripts/cmd-util.pl"; }

      ${if builtins.isString code
        then code
        else (if builtins.isPath code
             then builtins.readFile code
             else throw "code must be a string (literal code) or path to file")}
    '');
}
