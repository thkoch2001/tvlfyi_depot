{ depot, pkgs, ... }@args:

let
  inherit (import ../builder.nix args) buildGerritBazelPlugin;

# https://gerrit.googlesource.com/plugins/owners
in buildGerritBazelPlugin rec {
  name = "owners";
  depsOutputHash = "sha256:1jlwmhcfncpyiia6sqjjsgq6dp69xn29dzavhk2nr1ns7fzalvs5";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/owners";
    rev = "098e27cc0820c3e3985ca051bb18ad24bb5ebb32";
    sha256 = "sha256:12h8y5fj5w9njl9p5kvzh0cpjxsba6qf7pkigvczfr5kq9sqlb2f";
    postFetch = ''
      pushd $out
      patch -p1 < ${./0001-Adapt-to-changes-in-Gerrit-core.patch}
      popd
    '';
  };
  overlayPluginCmd = ''
    chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
    cp -R "${src}/owners" "$out/plugins/owners"
    cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
    cp -R "${src}/owners-common" "$out/owners-common"
  '';
}
