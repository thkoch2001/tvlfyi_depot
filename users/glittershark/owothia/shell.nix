{ pkgs ? (import ../../../. {}).third_party, ... }:

let
  inherit (pkgs)
    haskellPackages
    haskell
    gitignoreSource
    ;
in

(haskellPackages.extend (haskell.lib.packageSourceOverrides {
  owothia = gitignoreSource ./.;
})).shellFor {
  packages = p: [ p.owothia ];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = with haskellPackages; [
    cabal-install
    hlint
    haskell-language-server
  ];
}
