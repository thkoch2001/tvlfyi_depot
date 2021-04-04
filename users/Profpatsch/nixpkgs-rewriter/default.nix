{ depot, pkgs, ... }:
let
  inherit (depot.nix)
    writeExecline
    ;
  inherit (depot.users.Profpatsch.lib)
    debugExec
    ;

  bins = depot.nix.getBins pkgs.coreutils [ "head" "shuf" ]
      // depot.nix.getBins pkgs.jq [ "jq" ]
      // depot.nix.getBins pkgs.findutils [ "xargs" ]
      // depot.nix.getBins pkgs.gnused [ "sed" ]
      ;

  export-json-object = pkgs.writers.writePython3 "export-json-object" {} ''
    import json
    import sys
    import os

    d = json.load(sys.stdin)

    if d == {}:
        sys.exit(0)

    for k, v in d.items():
        os.environ[k] = str(v)

    os.execvp(sys.argv[1], sys.argv[1:])
  '';

  meta-stdenv-lib = pkgs.writers.writeHaskell "meta-stdenv-lib" {
    libraries = [
      pkgs.haskellPackages.hnix
      pkgs.haskellPackages.aeson
    ];
  } ./MetaStdenvLib.hs;

  replace-between-lines = writeExecline "replace-between-lines" { readNArgs = 1; } [
    "importas" "-ui" "file" "fileName"
    "importas" "-ui" "from" "fromLine"
    "importas" "-ui" "to" "toLine"
    "if" [ depot.tools.eprintf "%s-%s\n" "$from" "$to" ]
    (debugExec "adding lib")
    bins.sed
      "-e" "\${from},\${to} \${1}"
      "-i" "$file"
  ];

  add-lib-if-necessary = writeExecline "add-lib-if-necessary" { readNArgs = 1; } [
    "pipeline" [ meta-stdenv-lib "$1" ]
     export-json-object
     # first replace any stdenv.lib mentions in the arg header
     # if this is not done, the replace below kills these.
     # Since we want it anyway ultimately, let’s do it here.
     "if" [ replace-between-lines "s/stdenv\.lib/lib/" ]
     # then add the lib argument
     # (has to be before stdenv, otherwise default arguments might be in the way)
     replace-between-lines "s/stdenv/lib, stdenv/"
  ];

  metaString = ''meta = with stdenv.lib; {'';

  replace-stdenv-lib = pkgs.writers.writeBash "replace-stdenv-lib" ''
    set -euo pipefail
    sourceDir="$1"
    for file in $(
      ${pkgs.ripgrep}/bin/rg \
        --files-with-matches \
        --fixed-strings \
        -e '${metaString}' \
        "$sourceDir"
    )
    do
      echo "replacing stdenv.lib meta in $file" >&2
      ${bins.sed} -e '/${metaString}/ s/stdenv.lib/lib/' \
          -i "$file"
      ${add-lib-if-necessary} "$file"
    done
  '';

  instantiate-nixpkgs-randomly = writeExecline "instantiate-nixpkgs-randomly" { readNArgs = 1; } [
    "export" "NIXPKGS_ALLOW_BROKEN" "1"
    "export" "NIXPKGS_ALLOW_UNFREE" "1"
    "export" "NIXPKGS_ALLOW_INSECURE" "1"
    "export" "NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM" "1"
    "pipeline" [
      "nix"
        "eval"
        "--raw"
        ''(
          let pkgs = import ''${1} {};
          in builtins.toJSON (builtins.attrNames pkgs)
        )''
    ]
    "pipeline" [ bins.jq "-r" ".[]" ]
    "pipeline" [ bins.shuf ]
    "pipeline" [ bins.head "-n" "1000" ]
    bins.xargs "-I" "{}" "-n1"
    "if" [ depot.tools.eprintf "instantiating %s\n" "{}" ]
    "nix-instantiate" "$1" "-A" "{}"
  ];

in depot.nix.utils.drvTargets {
  inherit
   instantiate-nixpkgs-randomly
  # requires hnix, which we don’t want in tvl for now
  # uncomment manually if you want to use it.
  #   meta-stdenv-lib
  #   replace-stdenv-lib
    ;
}
