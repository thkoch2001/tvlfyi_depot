{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in depot.nix.readTree.drvTargets {
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "sha256:1jlwmhcfncpyiia6sqjjsgq6dp69xn29dzavhk2nr1ns7fzalvs5";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/owners";
      rev = "99a9ab585532d172d141b4641dfc70081513dfc2";
      sha256 = "sha256:1xn9qb7q94jxfx7yq0zjqjm16gfyzzif13sak9x6j4f9r68frcd4";
    };
    overlayPluginCmd = ''
      chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
      cp -R "${src}/owners" "$out/plugins/owners"
      cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
      cp -R "${src}/owners-common" "$out/owners-common"
    '';
  };
}
