{ depot, pkgs, ... }:
let
  inherit (depot.nix)
    writeExecline
    ;
  inherit (depot.users.Profpatsch.lib)
    debugExec
    eprintf
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
    "if" [ eprintf "\${from}-\${to}" ]
    (debugExec "adding lib")
    "sed"
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
      sed -e '/${metaString}/ s/stdenv.lib/lib/' \
          -i "$file"
      ${add-lib-if-necessary} "$file"
    done
  '';

in {
  # requires hnix, which we don’t want in tvl for now
  # uncomment manually if you want to use it.
  # inherit
  #   meta-stdenv-lib
  #   replace-stdenv-lib
  #   ;
}
