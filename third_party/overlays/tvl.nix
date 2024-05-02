# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ lib, depot, localSystem, ... }:

self: super:
depot.nix.readTree.drvTargets {
  nix_2_3 = (super.nix_2_3.override {
    # flaky tests, long painful build, see https://github.com/NixOS/nixpkgs/pull/266443
    withAWS = false;
  });
  nix = self.nix_2_3 // {
    # avoid duplicate pipeline step
    meta = self.nix_2_3.meta or { } // {
      ci = self.nix_2_3.meta.ci or { } // {
        skip = true;
      };
    };
  };
  nix_latest = super.nix.override ({
    # flaky tests, long painful build, see https://github.com/NixOS/nixpkgs/pull/266443
    withAWS = false;
  });

  # To match telega in emacs-overlay or wherever
  tdlib = super.tdlib.overrideAttrs (_: {
    version = "1.8.24";
    src = self.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "d79bd4b69403868897496da39b773ab25c69f6af";
      sha256 = "0bc5akzw12qwj45rzqkrhw65qlrn9q8pzmvc5aiqv4bvhkb1ghl0";
    };
  });

  home-manager = super.home-manager.overrideAttrs (_: {
    src = depot.third_party.sources.home-manager;
    version = "git-"
      + builtins.substring 0 7 depot.third_party.sources.home-manager.rev;
  });

  # Add our Emacs packages to the fixpoint
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope (eself: esuper: {
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

      # Pin xelb to a newer one until the new maintainers do a release.
      xelb = eself.trivialBuild {
        pname = "xelb";
        version = "0.19-dev"; # invented version, last actual release was 0.18

        src = self.fetchFromGitHub {
          owner = "emacs-exwm";
          repo = "xelb";
          rev = "86089eba2de6c818bfa2fac075cb7ad876262798";
          sha256 = "1mmlrd2zpcwiv8gh10y7lrpflnbmsycdascrxjr3bfcwa8yx7901";
        };
      };
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

  # https://github.com/googleapis/google-cloud-go/pull/9665
  cbtemulator = super.cbtemulator.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      ./patches/cbtemulator-uds.patch
    ];
  });

  crate2nix = super.crate2nix.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
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

  # Imports a patch that fixes usage of this package on versions
  # >=1.9. The patch has been proposed upstream, but so far with no
  # reactions from the maintainer:
  #
  # https://github.com/tpm2-software/tpm2-pkcs11/pull/849
  tpm2-pkcs11 = super.tpm2-pkcs11.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./patches/tpm2-pkcs11-190-dbupgrade.patch ];
  });
}
