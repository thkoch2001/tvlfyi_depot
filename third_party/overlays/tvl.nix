# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ lib, depot, localSystem, ... }:

self: super:
let
  # Rollback Nix to a stable version (2.3) with some backports applied.
  # We currently track a commit on the 2.3-maintenance that didn't make it into
  # a release yet - tracked in https://github.com/NixOS/nix/issues/9244.
  nixSrc =
    let
      # branch 2.3-maintenance
      rev = "f76990444c17716506080e5445e430a9c5c880f9";
    in
    self.fetchFromGitHub
      {
        owner = "NixOS";
        repo = "nix";
        inherit rev;
        hash = "sha256-EK0pgHDekJFqr0oMj+8ANIjq96WPjICe2s0m4xkUdH4=";
      } // { revCount = 0; shortRev = builtins.substring 0 7 rev; };

  nixTarball = (scopedImport
    {
      # The tarball job always uses currentSystem which we need to purify
      builtins = builtins // { currentSystem = localSystem; };
    }
    "${nixSrc}/release.nix"
    {
      nix = nixSrc;
      nixpkgs = self.path;
      systems = [ ];
    }
  ).tarball;
in
depot.nix.readTree.drvTargets {
  nix_2_3 = super.nix_2_3.overrideAttrs (_: {
    src = "${nixTarball}/tarballs/nix-${nixTarball.version}.tar.xz";
  });
  nix = self.nix_2_3;
  nix_latest = super.nix;

  # To match telega in emacs-overlay or wherever
  tdlib = super.tdlib.overrideAttrs (_: {
    version = "1.8.16";
    src = self.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "cde095db6c75827fe4bd237039574aad373ad96b";
      sha256 = "1zzacz2mhjmc36aqdc5v8a7zdi0mi7k8hnrnpj9gii061pm4vx4v";
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

  # stdenv which uses clang, lld and libc++; full is a slight exaggeration,
  # we for example don't use LLVM's libunwind
  fullLlvm11Stdenv = self.overrideCC self.stdenv
    (self.llvmPackages_11.libcxxStdenv.cc.override {
      inherit (self.llvmPackages_11) bintools;
    });

  # Add our Emacs packages to the fixpoint
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope' (eself: esuper: {
      tvlPackages = depot.tools.emacs-pkgs // depot.third_party.emacs;

      # Use the notmuch from nixpkgs instead of from the Emacs
      # overlay, to avoid versions being out of sync.
      notmuch = super.notmuch.emacs;

      # Build EXWM with the depot sources instead.
      depotExwm = esuper.exwm.overrideAttrs (_: {
        src = depot.path.origSrc + "/third_party/exwm";
      });

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

  crate2nix = super.crate2nix.overrideAttrs (old: rec {
    patches = old.patches ++ [
      # run tests in debug mode, not release mode
      # https://github.com/nix-community/crate2nix/pull/301
      ./patches/crate2nix-tests-debug.patch

      # https://github.com/nix-community/crate2nix/pull/309
      ./patches/crate2nix-take-lndir-from-buildPackages.patch
      ./patches/crate2nix-skip-running-tests-when-cross-compiling.patch
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

  clickhouse = super.clickhouse.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      # https://github.com/ClickHouse/ClickHouse/pull/56118
      ./patches/clickhouse-support-reading-arrow-LargeListArray.patch
    ];
  });
}
