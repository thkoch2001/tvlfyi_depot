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

  # Upstreamed in https://github.com/NixOS/nixpkgs/pull/298044,
  # can be reduced to just the patch on the next nixpkgs bump.
  cbtemulator = super.buildGoModule rec {
    pname = "cbtemulator";
    version = "1.22.0";

    src = (super.fetchFromGitHub {
      owner = "googleapis";
      repo = "google-cloud-go";
      rev = "bigtable/v${version}";
      hash = "sha256-eOi4QFthnmZb5ry/5L7wzr4Fy1pF/H07BzxOnXtmSu4=";
    }) + "/bigtable";

    vendorHash = "sha256-7M7YZfl0usjN9hLGozqJV2bGh+M1ec4PZRGYUhEckpY=";
    subPackages = [ "cmd/emulator" ];

    patches = [
      ./patches/cbtemulator-uds.patch
    ];

    postInstall = ''
      mv $out/bin/emulator $out/bin/cbtemulator
    '';

    meta = with lib; {
      description = "In-memory Google Cloud Bigtable server";
      homepage = "https://github.com/googleapis/google-cloud-go/blob/bigtable/v1.22.0/bigtable/cmd/emulator/cbtemulator.go";
      license = licenses.asl20;
      maintainers = [ maintainers.flokli ];
      mainProgram = "cbtemulator";
      platforms = platforms.all;
    };
  };

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

  # Imports a patch that fixes usage of this package on versions
  # >=1.9. The patch has been proposed upstream, but so far with no
  # reactions from the maintainer:
  #
  # https://github.com/tpm2-software/tpm2-pkcs11/pull/849
  tpm2-pkcs11 = super.tpm2-pkcs11.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ./patches/tpm2-pkcs11-190-dbupgrade.patch ];
  });
}
