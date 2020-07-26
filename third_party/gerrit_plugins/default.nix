{ depot, pkgs, ... }:

let
  inherit (pkgs) fetchurl;

  buildGerritBazelPlugin = attrs: ((depot.third_party.gerrit.override rec {
    name = "${attrs.name}.jar";

    overlayPluginCmd = pkgs.lib.attrByPath ["overlayPluginCmd"] ''
      cp -R "${attrs.src}" "$out/plugins/${attrs.name}"
    '' attrs;

    src = pkgs.runCommandLocal "${attrs.name}-src" {} ''
      cp -R "${depot.third_party.gerrit.src}" "$out"
      chmod +w "$out/plugins"
      ${overlayPluginCmd}
    '';

    bazelTarget = "//plugins/${attrs.name}";
  }).overrideAttrs (super: {
    deps = super.deps.overrideAttrs (superDeps: {
      outputHash = attrs.depsOutputHash;
    });
    installPhase = ''
      cp "bazel-bin/plugins/${attrs.name}/${attrs.name}.jar" "$out"
    '';
  }));
in {
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

  # https://gerrit.googlesource.com/plugins/checks
  checks = buildGerritBazelPlugin {
    name = "checks";
    depsOutputHash = "1nk63ahlynqn5my7lzy7vsax07n585ndmvbnz2xbwkppl1m7j56l";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/checks";
      rev = "36d9e5c61b95ade98883ded5eb1e168c28ef99bf";
      sha256 = "03abn4m9x8cinx86c41hrazg0kgwvjfak3w155sca1ikq4605pma";
      deepClone = true;
      leaveDotGit = true;
    };
  };
}
