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

  configWrapper = pkgs.runCommand "eaglemode-config-wrapper" { } ''
    cp ${./wrapper.go} wrapper.go
    export HOME=$PWD
    ${pkgs.go}/bin/go build wrapper.go
    install -Dm755 wrapper $out/bin/wrapper
  '';
in
rec {
  # mkCommand creates an Eagle Mode command for the file browser.
  #
  # Commands are basically little Perl scripts with a command standard library
  # available. They receive the user's selected target from Eagle Mode.
  mkCommand = lib.makeOverridable (
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
    '')
  );

  # mkTGA converts the given image to a TGA image.
  mkTGA = name: path: pkgs.runCommand "${name}.tga" { } ''
    ${pkgs.imagemagick}/bin/convert ${path} $out
  '';

  buildPlugin = lib.makeOverridable (
    { name
    , src
    , version
    , eaglemode ? pkgs.eaglemode
    , target ? name
    , extraNativeBuildInputs ? [ ]
    , extraBuildInputs ? [ ]
    }:
    pkgs.stdenv.mkDerivation {
      pname = "eaglemode-plugin-${name}";
      inherit src version;
      # inherit (eaglemode.drvAttrs) dontPatchELF;

      nativeBuildInputs = eaglemode.drvAttrs.nativeBuildInputs ++ extraNativeBuildInputs;
      buildInputs = eaglemode.drvAttrs.buildInputs ++ extraBuildInputs ++ [ eaglemode ];

      buildPhase = ''
        runHook preBuild

        # merge eaglemode & plugin folders
        cp -r ${pkgs.srcOnly eaglemode} merged-src && chmod -R u+rw merged-src
        cp -r $src/* merged-src && chmod -R u+rw merged-src
        cd merged-src

        export NIX_LDFLAGS="$NIX_LDFLAGS -lXxf86vm -lXext -lXinerama"
        perl make.pl build projects=${target} continue=no

        runHook postBuild
      '';

      installPhase = ''
        runHook preInstall

        mkdir -p $out/lib
        cp -r lib/lib${target}.so $out/lib

        if [ -d "$src/etc" ]; then
          cp -r $src/etc/* $out
        fi

        runHook postInstall
      '';
    }
  );

  # etcDir creates a directory layout suitable for use in the EM_USER_CONFIG_DIR
  # environment variable.
  #
  # Note that Eagle Mode requires the value of that variable to be mutable at
  # runtime (it is the same place where it persists all of its user-controlled
  # state), so the results of this function can not be used directly.
  etcDir =
    { eaglemode ? pkgs.eaglemode
    , extraPaths ? [ ]
    ,
    }: pkgs.runCommand "eaglemode-config" { } ''
      mkdir $out

      ${
        lib.concatMapStringsSep "\n" (s: "cp -rT ${s} $out/\nchmod -R u+rw $out/\n") ([ "${eaglemode}/etc"] ++ extraPaths)
      }
    '';

  # withConfig creates an Eagle Mode wrapper that runs it with the given
  # configuration.
  withConfig = { eaglemode ? pkgs.eaglemode, config }: pkgs.writeShellScriptBin "eaglemode" ''
    ${configWrapper}/bin/wrapper --em-config "${config}"

    if [ -d "${config}/lib" ]; then
      export LD_LIBRARY_PATH="${config}/lib:$LD_LIBRARY_PATH"
      exec ${eaglemode}/bin/eaglemode "$@"
    fi

    exec ${eaglemode}/bin/eaglemode "$@"
  '';
}
