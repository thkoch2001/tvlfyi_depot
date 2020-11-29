{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in
{
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "sha256:1liya9ayk6wv7yc0fpv0vyx7pnvnxirxnsxybs4akgbmrss5ahs2";
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

  # https://gerrit.googlesource.com/plugins/checks
  checks = buildGerritBazelPlugin {
    name = "checks";
    depsOutputHash = "sha256:12yg72w7kndz5ag7cgdzhxdpv8hy3qln71n93226iswwkn22c2sw";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/checks";
      rev = "ab49a63f5c159bda42d9ad1bdb9286bede6c5de4";
      sha256 = "sha256:0v16hrpfppi4hi2l2133m56fknbvc3nas1h1a1x48gdgm8d4g27f";
      deepClone = true;
      leaveDotGit = true;
    };
  };

  # https://gerrit.googlesource.com/plugins/avatars-gravatar
  avatars-gravatar = buildGerritBazelPlugin {
    name = "avatars-gravatar";
    depsOutputHash = "sha256:0svnzfq1zbqwrjik760j350xr4vs5n5y5gnlz2kh9c60knc35rpq";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/avatars-gravatar";
      rev = "85d9b12f003f801717c79d7546a9044c527f6f92";
      sha256 = "sha256:1lrdadky3qh6aw89rj9bwrxs9xghi5vjwxb5jhmsknilv1g31zxs";
      leaveDotGit = true;
    };
  };
}
