{ depot, pkgs, ... }@args:

let
  inherit (import ../builder.nix args) buildGerritBazelPlugin;
in
buildGerritBazelPlugin rec {
  name = "oauth";
  version = "982316";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/oauth";
    rev = "98231604d60788bb43490f1a301d792817ac8008";
    hash = "sha256-AuVO1Yys8BYqGHZI/adszCUg0JM2v4Td4fe26LdOPLM=";
  };
  depsHash = "sha256-7SC4NXm4zGeJrYBqtEvcrLmsZmXEX8P21J0kwHBDBZ4=";
  postOverlayPlugin = ''
    cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
  '';
}
