let
  pkgs = import <unstable> {};
in pkgs.mkShell {
  buildInputs = with pkgs; [
    ghc
    haskellPackages.time
  ];
}
