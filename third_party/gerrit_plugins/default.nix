{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in
{
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "1l99a1qa3bndk1cx4fsrq18z3p7rg6yp9r83snldp97kg25873cs";
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
    depsOutputHash = "0igixhkgvd8d51swbypsby553sdzghlj4b9wy9cnf2d8d0aqs2ky";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/checks";
      rev = "f508353fc5f93d35d07279857aba5d9cf38de658";
      sha256 = "094j225cpgckxvnqzk3rif34iylgr0r7v4n6r1m1qymavbk9wg9j";
      deepClone = true;
      leaveDotGit = true;
    };
  };
}
