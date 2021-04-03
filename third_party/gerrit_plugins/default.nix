{ depot, pkgs, ... }@args:

let
  inherit (import ./builder.nix args) buildGerritBazelPlugin;
in depot.nix.utils.drvTargets {
  # https://gerrit.googlesource.com/plugins/owners
  owners = buildGerritBazelPlugin rec {
    name = "owners";
    depsOutputHash = "sha256:0j7hn945l5y5pz109mrcx2hh2lb2gi5gf4wrrbypx43rmyhlz3s8";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/owners";
      rev = "f3335231b98e14664fdd1b325486bb0824800ac3";
      sha256 = "sha256:0dqf36wn6gnkwia3529dwlcib2np78dgsjs7dymg5isy1i8p655f";
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
    depsOutputHash = "sha256:01krrafg5df42z3r7y74g8lx859my4610cqx3a7d02laqq9yjqc6";
    src = pkgs.fetchgit {
      url = "https://gerrit.googlesource.com/plugins/checks";
      rev = "990e936b1e050c4fe7ac3e590bdb5cfff0311232";
      sha256 = "sha256:0cgrwrimsxx0dnqvp0akv5pz17hy743lhkqvsks60fijb34ps54s";
    };
  };
}
