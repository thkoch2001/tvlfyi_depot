{ depot, pkgs, ... }@args:

let
  inherit (import ../builder.nix args) buildGerritBazelPlugin;
in
buildGerritBazelPlugin rec {
  name = "oauth";
  depsOutputHash = "sha256:01z7rn8hnms3cp7mq27yk063lpy4pmqwpfrcc3cfl0r43k889zz3";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/oauth";
    rev = "b27cf3ea820eec2ddd22d217fc839261692ccdb0";
    sha256 = "1m654ibgzprrhcl0wpzqrmq8drpgx6rzlw0ha16l1fi2zv5idkk2";
  };
  overlayPluginCmd = ''
    chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
    cp -R "${src}" "$out/plugins/${name}"
    cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
  '';
}
