{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in depot.nix.utils.drvTargets {
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "sha256:1cffbbn687dcl46jm70fd5h6an7jwalssvy0y1aqnhrwmbr4gwi9";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/owners";
      rev = "17817c9e319073c03513f9d5177b6142b8fd567b";
      sha256 = "sha256:06j1wp5xpwabmzyhjjl5rcmgsxqi8ffzgzf8avbr61qzgh76f22n";
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
    depsOutputHash = "sha256:1s4ldzny9f8vpp96sff8zy2kigz3hi560ikrgyp5630ffrc8affq";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/checks";
      rev = "ab49a63f5c159bda42d9ad1bdb9286bede6c5de4";
      sha256 = "sha256:0plvgx61pwksfdr4fpclzm9pxrn4pcydk08jp4y60qd3qlydks3j";
    };
  };
}
