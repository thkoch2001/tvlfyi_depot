{ depot, ... }:

{ nixpkgsPath ? depot.third_party.nixpkgs.path
, nixpkgsArgs ? {}
, asDerivation ? true
}@args:

let
  inherit (depot.third_party.nixpkgs)
    lib
    runCommandNoCC
    writeText
  ;

  inherit (depot.nix)
    netstring
  ;

  pkgs = import nixpkgsPath nixpkgsArgs;

  /* Condition for us to recurse:
     Either we are at the top-level or
     recurseForDerivations is true.

     Type :: list any -> any -> bool
  */
  recurseInto = path: x: path == [] ||
    (lib.isAttrs x && (x.recurseForDerivations or false));

  /* `tryEval`s a boolean, returning `false` if it throws on evaluation,
     the boolean otherwise.

     Type :: (bool | eval error) -> bool
  */
  tryEvalOrFalse = v: (builtins.tryEval v).value;

  /* Predicate by which we identify rust packages we are interested in,
     i. e. built using `buildRustPackage`.

     Type :: drv -> bool
  */
  isRustPackage = v: v ? cargoDeps;

  /* Takes a buildRustPackage derivation and returns a derivation which
     builds extracts the `Cargo.lock` of its `cargoDeps` derivation or
     `null` if it has none.

     Type: drv -> option<drv>
  */
  # TODO(sterni): support cargoVendorDir?
  extractCargoLock = drv:
    if !(drv ? cargoDeps.outPath)
    then null
    else runCommandNoCC "${drv.name}-Cargo.lock" {} ''
      if test -d "${drv.cargoDeps}"; then
        cp "${drv.cargoDeps}/Cargo.lock" "$out"
      fi

      if test -f "${drv.cargoDeps}"; then
        tar -xO \
          --no-wildcards-match-slash --wildcards \
          -f "${drv.cargoDeps}" \
          '*/Cargo.lock' \
          > "$out"
      fi
    '';

  /* Manipulates the context of a string, so that every dependency is converted
     into a `{ path = true; }` dependency. This is useful when generating a
     string full of derivation paths, so that the resulting string will only
     depend on the derivations and not their outputs.

     Type: str -> str
  */
  onlyPathContext = str:
    let
      context = lib.mapAttrs (_: _: {
        path = true;
      }) (builtins.getContext str);
    in
      builtins.appendContext (builtins.unsafeDiscardStringContext str) context;

  /* Traverses nixpkgs as instructed by `recurseInto` and collects
     the attribute and lockfile derivation of every rust package it
     encounters into a list.

     Type :: attrs -> list { attr :: string; lock :: option<drv>; }
  */
  rustPkgs =
    let
      go = path: x:
        let
          isDrv = tryEvalOrFalse (lib.isDerivation x);
          doRec = tryEvalOrFalse (recurseInto path x);
          isRust = tryEvalOrFalse (isRustPackage x);
        in
          if doRec then lib.concatLists (
            lib.mapAttrsToList (n: go (path ++ [ n ])) x
          ) else if isDrv && isRust then [
            {
              attr = lib.concatStringsSep "." path;
              lock = onlyPathContext ((extractCargoLock x).drvPath or "");
            }
          ] else [];
    in go [];

  /* `rustPkgs`, but formatted for consumption by execline:
     All attrset are formatted as netstrings, one per line.

     Type: str
  */
  netstrings =
    lib.concatMapStrings ({ attr, lock }: ''
      ${netstring.fromString attr}${netstring.fromString lock}
    '') (rustPkgs pkgs);
in

if asDerivation
then writeText "all-nixpkgs-Cargo.locks" netstrings
else netstrings
