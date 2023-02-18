# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ depot, localSystem, ... }:

self: super:
let
  # Rollback Nix to a stable version (2.3) with backports for
  # build-user problems applied.
  nixSrc =
    let
      # branch 2.3-backport-await-users
      rev = "46a91f2abaee85135cbb447c13957d3c5e9091a9";
    in
    self.fetchFromGitHub
      {
        owner = "tvlfyi";
        repo = "nix";
        inherit rev;
        hash = "sha256:0rwyrh471c5y64axyd8vzzzmzlscg97fsrjbgbm1a93wnzxcvnvk";
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

  nvd = super.nvd.overrideAttrs (old: {
    patches = old.patches or [ ] ++ [
      ./patches/nvd-nix-2.3.patch
    ];
  });

  # Too match telega in emacs-overlay or wherever
  tdlib = super.tdlib.overrideAttrs (_: {
    version = "1.8.11";
    src = self.fetchFromGitHub {
      owner = "tdlib";
      repo = "td";
      rev = "1543c41f3411bd6aa74713c8aba4e93fa8d952c7";
      sha256 = "0qw3a2wh5hfad0m4ixywh5p2mvyprkw982jb7n4sqxdvwc0xfcgq";
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
      exwm = esuper.exwm.overrideAttrs (_: {
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
  nix-serve = super.nix-serve.override { nix = super.nix_2_3; };

  # Avoid builds of mkShell derivations in CI.
  mkShell = super.lib.makeOverridable (args: (super.mkShell args).overrideAttrs (_: {
    passthru = {
      meta.ci.skip = true;
    };
  }));

  # Pin a newer version of crate2nix from git, which is not officially
  # released but supports `dep:`
  crate2nix = super.crate2nix.overrideAttrs (old: rec {
    version = "0.11.0-rc.1";

    src = self.fetchFromGitHub {
      owner = "kolloch";
      repo = "crate2nix";
      rev = "v0.11.0-rc.1";
      hash = "sha256:02yvn61w16sgkdxa019l5y9i2ybyk8h4516718gmarqxx5ws2kz8";
    };

    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "sha256:03yny9ikqzfpi2mr27r82g54an2s8k9lqi2i4fqalg7g0s2cr2yd";
    });
  });
}
