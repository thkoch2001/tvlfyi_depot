{ pkgs, depot, lib, ... }:
let

  # import the dhall file as nix expression via dhall-nix.
  # Converts the normalized dhall expression to a nix file,
  # puts it in the store and imports it.
  # Types are erased, functions are converted to nix functions,
  # unions values are nix functions that take a record of match
  # functions for their alternatives.
  # TODO: document better
  importDhall = { root, files, main, deps, type }:
    let
      src =
        depot.users.Profpatsch.exactSource
          root
          # exactSource wants nix paths, but I think relative paths
          # as strings are more intuitive.
          (let abs = path: toString root + "/" + path;
           in ([ (abs main) ] ++ (map abs files)));

      cache = ".cache";
      cacheDhall = "${cache}/dhall";

      convert = pkgs.runCommandLocal "dhall-to-nix" { inherit deps; } ''
        mkdir -p ${cacheDhall}
        for dep in $deps; do
          ${pkgs.xorg.lndir}/bin/lndir -silent $dep/${cacheDhall} ${cacheDhall}
        done

        export XDG_CACHE_HOME=$(pwd)/${cache}
        # go into the source directory, so that the type can import files.
        # TODO: This is a bit of a hack hrm.
        cd "${src}"
        printf '%s' ${lib.escapeShellArg "${src}/${main} : ${type}"} \
          | ${pkgs.dhall-nix}/bin/dhall-to-nix \
          > $out
      '';
    in import convert;


  # read dhall file in as JSON, then import as nix expression.
  # The dhall file must not try to import from non-local URLs!
  readDhallFileAsJson = dhallType: file:
    let
      convert = pkgs.runCommandLocal "dhall-to-json" {} ''
        printf '%s' ${lib.escapeShellArg "${file} : ${dhallType}"} \
          | ${pkgs.dhall-json}/bin/dhall-to-json \
          > $out
      '';
    in builtins.fromJSON (builtins.readFile convert);

in {
  inherit
    importDhall
    readDhallFileAsJson
    ;
}
