args@{ pkgs ? (import ../../../. {}).third_party, ... }:

((import ./packageSet.nix args).extend (pkgs.haskell.lib.packageSourceOverrides {
  owothia = pkgs.gitignoreSource ./.;
})).shellFor {
  packages = p: [p.owothia];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = with pkgs.haskellPackages; [
    cabal-install
    hlint
    pkgs.haskell-language-server.ghc884
  ];
}
