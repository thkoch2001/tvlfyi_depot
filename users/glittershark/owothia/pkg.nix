args@{ pkgs ? (import ../../../. {}).third_party }:

import ((import ./packageSet.nix args).haskellSrc2nix {
  name = "owothia";
  src = pkgs.gitignoreSource ./.;
})
