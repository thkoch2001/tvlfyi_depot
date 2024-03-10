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

  crate2nix = super.rustPlatform.buildRustPackage rec {
    pname = "crate2nix";
    version = "0.13.0";

    src = super.fetchFromGitHub {
      owner = "nix-community";
      repo = "crate2nix";
      rev = "ceb06eb7e76afb9e01a5f069aae136f97df72730";
      hash = "sha256-JTMe8GViCQt51WUiaaoIPmWtwEeeYrl6pBxo2DNuKig=";
    };

    patches = [
      ./patches/crate2nix-tests-debug.patch
      ./patches/crate2nix-run-tests-in-build-source.patch
    ];

    sourceRoot = "${src.name}/crate2nix";

    cargoHash = "sha256-dhlSXY1CJE+JJt+6Y7W1MVMz36nwr6ny543py1TcjyY=";

    nativeBuildInputs = [ super.makeWrapper ];

    # Tests use nix(1), which tries (and fails) to set up /nix/var inside the
    # sandbox
    doCheck = false;

    postFixup = ''
      wrapProgram $out/bin/crate2nix \
          --suffix PATH ":" ${lib.makeBinPath (with self; [ cargo nix_latest nix-prefetch-git ])}

      rm -rf $out/lib $out/bin/crate2nix.d
      mkdir -p \
        $out/share/bash-completion/completions \
        $out/share/zsh/vendor-completions
      $out/bin/crate2nix completions -s 'bash' -o $out/share/bash-completion/completions
      $out/bin/crate2nix completions -s 'zsh' -o $out/share/zsh/vendor-completions
    '';
  };

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

  # OpenVPN + TPM2 is broken on versions of this package somewhere
  # after 1.8.0, but it is a critical dependency for tazjin. For this
  # reason it is vendored from a specific nixpkgs commit.
  tpm2-pkcs11 = self.callPackage ./patches/tpm2-pkcs11.nix { };
}
