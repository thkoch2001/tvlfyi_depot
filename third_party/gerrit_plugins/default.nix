{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in
{
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "sha256:14h39q37lg9bydjqnhr52jakqvrx97jfqfg03l35qyadw780pny2";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/owners";
      rev = "17817c9e319073c03513f9d5177b6142b8fd567b";
      sha256 = "sha256:1p089shybp50svckcq51d0hfisjvbggndmvmhh8pvzvi6w8n9d89";
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
    depsOutputHash = "sha256:1y1mfnznqrm73vf97ilzn7vjy3fcfqnz9jxgp6h5584k9adnb6zl";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/checks";
      rev = "ab49a63f5c159bda42d9ad1bdb9286bede6c5de4";
      sha256 = "0ghhmzkfmpn0xc6imvxgsk2wqckxm5g62h0k3bymyl7r48blmrn2";
      deepClone = true;
      leaveDotGit = true;
    };
  };
}
