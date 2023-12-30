# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ lib, depot, localSystem, ... }:

self: super:
depot.nix.readTree.drvTargets {
  nix_2_3 = (super.nix_2_3.override {
    # flaky tests, long painful build, see https://github.com/NixOS/nixpkgs/pull/266443
    withAWS = false;
  });
  nix = self.nix_2_3;
  nix_latest = super.nix.override ({
    # flaky tests, long painful build, see https://github.com/NixOS/nixpkgs/pull/266443
    withAWS = false;
  });

  # To match telega in emacs-overlay or wherever
  tdlib = super.tdlib.overrideAttrs (_: {
    version = "1.8.23";
    src = self.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "27c3eaeb4964bd5f18d8488e354abde1a4383e49";
      sha256 = "14f65dfmg2p5hyvi3lffvvazwcd3i3jrrw3c2pwrc5yfgxk3662g";
    };
  });

  home-manager = super.home-manager.overrideAttrs (_: {
    src = depot.third_party.sources.home-manager;
    version = "git-"
      + builtins.substring 0 7 depot.third_party.sources.home-manager.rev;
  });

  clang-tools_11 = self.clang-tools.override {
    llvmPackages = self.llvmPackages_11;
  };

  # Add our Emacs packages to the fixpoint
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope' (eself: esuper: {
      tvlPackages = depot.tools.emacs-pkgs // depot.third_party.emacs;

      # Use the notmuch from nixpkgs instead of from the Emacs
      # overlay, to avoid versions being out of sync.
      notmuch = super.notmuch.emacs;

      # Build EXWM with the depot sources instead.
      depotExwm = eself.callPackage depot.third_party.exwm.override { };

      # Workaround for magit checking the git version at load time
      magit = esuper.magit.overrideAttrs (_: {
        propagatedNativeBuildInputs = [
          self.git
        ];
      });
    })
  );

  # dottime support for notmuch
  notmuch = super.notmuch.overrideAttrs (old: {
    passthru = old.passthru // {
      patches = old.patches ++ [ ./patches/notmuch-dottime.patch ];
    };
  });

  # nix-serve does not work with nix 2.4
  # https://github.com/edolstra/nix-serve/issues/28
  nix-serve = super.nix-serve.override { nix = self.nix_2_3; };

  # Avoid builds of mkShell derivations in CI.
  mkShell = super.lib.makeOverridable (args: (super.mkShell args).overrideAttrs (_: {
    passthru = {
      meta.ci.skip = true;
    };
  }));

  crate2nix = (super.crate2nix.override (_: { nix = self.nix_latest; })).overrideAttrs (old: rec {
    patches = old.patches ++ [
      # run tests in debug mode, not release mode
      # https://github.com/nix-community/crate2nix/pull/301
      ./patches/crate2nix-tests-debug.patch
    ];
  });

  evans = super.evans.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      # add support for unix domain sockets
      # https://github.com/ktr0731/evans/pull/680
      ./patches/evans-add-support-for-unix-domain-sockets.patch
    ];
  });

  # Package gerrit-queue, which is not in nixpkgs yet
  gerrit-queue = super.buildGoModule {
    pname = "gerrit-queue";
    version = "unstable-2023-10-20";
    vendorHash = "sha256-+Ig4D46NphzpWKXO23Haea9EqVtpda8v9zLPJkbe3bQ=";
    src = super.fetchFromGitHub {
      owner = "flokli";
      repo = "gerrit-queue";
      rev = "0186dbde15c9b11dc17b422feb74c842f6fa605a";
      hash = "sha256-zXB5vre/Vr7UOyeMnf2RCtMKm+v5RENH7kGPr/2o7mI=";
    };

    meta = with lib; {
      description = "Gerrit submit bot";
      homepage = "https://github.com/tweag/gerrit-queue";
      license = licenses.asl20;
    };
  };

  # we're vendoring this for now, since the version upgrade has a lot of changes
  # we can't merge it upstream yet because the Darwin build is broken
  # https://github.com/NixOS/nixpkgs/pull/267033
  clickhouse = (super.callPackage ./clickhouse {
    llvmPackages = super.llvmPackages_16;
  }).overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      # https://github.com/ClickHouse/ClickHouse/pull/56118
      ./patches/clickhouse-support-reading-arrow-LargeListArray.patch
    ];
  });
}
