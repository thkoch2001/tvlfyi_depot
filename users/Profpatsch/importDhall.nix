{ pkgs, depot, lib, ... }:
let

  # import the dhall file as nix expression via dhall-nix.
  # Converts the normalized dhall expression to a nix file,
  # puts it in the store and imports it.
  # Types are erased, functions are converted to nix functions,
  # unions values are nix functions that take a record of match
  # functions for their alternatives.
  # TODO: document better
  importDhall =
    {
      # Root path of the dhall file tree to import (will be filtered by files)
      root
    , # A list of files which should be taken from `root` (relative paths).
      # This is for minimizing the amount of things that have to be copied to the store.
      # TODO: can you have directory prefixes?
      files
    , # The path of the dhall file which should be evaluated, relative to `root`, has to be in `files`
      main
    , # List of dependencies (TODO: what is a dependency?)
      deps
    , # dhall type of `main`, or `null` if anything should be possible.
      type ? null
    }:
    let
      src =
        depot.users.Profpatsch.exactSource
          root
          # exactSource wants nix paths, but I think relative paths
          # as strings are more intuitive.
          (
            let abs = path: toString root + "/" + path;
            in ([ (abs main) ] ++ (map abs files))
          );

      cache = ".cache";
      cacheDhall = "${cache}/dhall";

      hadTypeAnnot = type != null;
      typeAnnot = lib.optionalString hadTypeAnnot ": ${type}";

      convert = pkgs.runCommandLocal "dhall-to-nix" { inherit deps; } ''
        mkdir -p ${cacheDhall}
        for dep in $deps; do
          ${pkgs.xorg.lndir}/bin/lndir -silent $dep/${cacheDhall} ${cacheDhall}
        done

        export XDG_CACHE_HOME=$(pwd)/${cache}
        # go into the source directory, so that the type can import files.
        # TODO: This is a bit of a hack hrm.
        cd "${src}"
        ${if hadTypeAnnot then ''
            printf '%s' ${lib.escapeShellArg "${src}/${main} ${typeAnnot}"} \
              | ${pkgs.dhall-nix}/bin/dhall-to-nix \
              > $out
          ''
          else ''
            printf 'No type annotation given, the dhall expression type was:\n'
            ${pkgs.dhall}/bin/dhall type --file "${src}/${main}"
            printf '%s' ${lib.escapeShellArg "${src}/${main}"} \
              | ${pkgs.dhall-nix}/bin/dhall-to-nix \
              > $out
          ''}

      '';
    in
    import convert;


  # read dhall file in as JSON, then import as nix expression.
  # The dhall file must not try to import from non-local URLs!
  readDhallFileAsJson = dhallType: file:
    let
      convert = pkgs.runCommandLocal "dhall-to-json" { } ''
        printf '%s' ${lib.escapeShellArg "${file} : ${dhallType}"} \
          | ${pkgs.dhall-json}/bin/dhall-to-json \
          > $out
      '';
    in
    builtins.fromJSON (builtins.readFile convert);

in
{
  inherit
    importDhall
    readDhallFileAsJson
    ;
}
