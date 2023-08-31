{ depot ? (import ../../../. { })
, pkgs ? depot.third_party.nixpkgs
, ...
}:

let
  ignore = depot.third_party.gitignoreSource.gitignoreFilter ./.;
  src = builtins.path {
    name = "xanthous-source";
    path = ./.;
    filter = path: type:
      !(type == "directory" && builtins.baseNameOf path == "server")
      && !(type == "directory" && builtins.baseNameOf path == "docs")
      && (ignore path type
      || builtins.baseNameOf path == "package.yaml");
  };
  # generated by cabal2nix
  basePkg = pkgs.haskellPackages.callPackage ./pkg.nix { };
in

pkgs.haskell.lib.overrideCabal basePkg (default: {
  inherit src;
  version = "canon";
  configureFlags = [
    "--ghc-option=-Wall"
    "--ghc-option=-Werror"
    "--ghc-option=-Wno-incomplete-uni-patterns"
    "--ghc-option=-fconstraint-solver-iterations=10"
  ] ++ (default.configureFlags or [ ]);
})
