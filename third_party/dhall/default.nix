{ pkgs, ... }:

let
  # binary releases of dhall tools, since the build in nixpkgs is
  # broken most of the time. The binaries are also fully static
  # builds, instead of the half-static crap that nixpkgs produces.
  easy-dhall-nix =
    import
      (builtins.fetchTarball {
        url = "https://github.com/justinwoo/easy-dhall-nix/archive/eae7f64c4d6c70681e5a56c84198236930ba425e.tar.gz";
        sha256 = "1y2x15v8a679vlpxazjpibfwajp6zph60f8wjcm4xflbvazk0dx7";
      })
      { inherit pkgs; };
in
{
  dhall = easy-dhall-nix.dhall-simple;
  dhall-bash = easy-dhall-nix.dhall-bash-simple;
  dhall-docs = easy-dhall-nix.dhall-docs-simple;
  dhall-json = easy-dhall-nix.dhall-json-simple;
  dhall-lsp-server = easy-dhall-nix.dhall-lsp-simple;
  dhall-nix = easy-dhall-nix.dhall-nix-simple;
  # not yet in dhall-simple
  # dhall-nixpkgs = easy-dhall-nix.dhall-nixpkgs-simple;
  dhall-yaml = easy-dhall-nix.dhall-yaml-simple;
}
