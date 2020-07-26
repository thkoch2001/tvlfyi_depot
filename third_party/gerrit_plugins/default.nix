{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in
{
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "0j60yn65kn27s7cjkj3z6irymq7j7rj3q5h3n6xfrs5inm4md2ad";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/owners";
      rev = "17817c9e319073c03513f9d5177b6142b8fd567b";
      sha256 = "1p089shybp50svckcq51d0hfisjvbggndmvmhh8pvzvi6w8n9d89";
      deepClone = true;
      leaveDotGit = true;
    };
    overlayPluginCmd = ''
      chmod +w "$out" "$out/plugins/external_plugin_deps.bzl"
      cp -R "${src}/owners" "$out/plugins/owners"
      cp "${src}/external_plugin_deps.bzl" "$out/plugins/external_plugin_deps.bzl"
      cp -R "${src}/owners-common" "$out/owners-common"
    '';
  };
}
