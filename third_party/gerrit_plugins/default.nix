{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in
depot.nix.readTree.drvTargets {
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "sha256:129k0jz2pxfl3yvdd95wvvkcjjmmw6jy4g45ss1pgvb9dan0ca6j";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/owners";
      rev = "070820d2df4f253c7c470f2cbe68da8043e163d0";
      sha256 = "sha256:0mw2fff3dca1xv2cvg9hsy5zw1h5gjidb5fh95x6yaj9g9lp0mxq";
    };
    overlayPluginCmd = ''
      chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
      cp -R "${src}/owners" "$out/plugins/owners"
      cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
      cp -R "${src}/owners-common" "$out/owners-common"
    '';
  };
}
