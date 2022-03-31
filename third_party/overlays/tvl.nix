# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ depot, ... }:

self: super: {
  # Rollback Nix to a stable version (2.3) while there is lots of
  # random ecosystem breakage with the newer versions.
  nix = super.nix_2_3;

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

  # bump nixpkgs-fmt to a version that doesn't touch whitespace in
  # strings
  nixpkgs-fmt = super.nixpkgs-fmt.overrideAttrs (old: rec {
    src = self.fetchFromGitHub {
      owner = "nix-community";
      repo = "nixpkgs-fmt";
      rev = "5ae8532b82eb040ca6b21ae2d02d9e88f604e76a";
      sha256 = "0hjkbcgz62793hzfzlaxyah8a2c1k79n1k891lg7kfw8mkbq0w4p";
    };

    cargoDeps = old.cargoDeps.overrideAttrs (_: {
      inherit src;
      outputHash = "10if2lmv8d95j3walq3ggx3y423yfy4yl9vplw3apd0s671bly8b";
    });
  });

  # Temporary workaround until the following commit is in channels:
  # https://github.com/nixos/nixpkgs/commit/5819b99350d7ac6e20f91adce38e7bb22e4d6fc4
  ocamlPackages = super.ocamlPackages.overrideScope' (oself: osuper: {
    uutf =
      let
        expectedUrls = [
          "https://erratique.ch/software/uutf/releases/uutf-${osuper.uutf.version}.tbz"
        ];
      in
      assert osuper.uutf.src.urls != expectedUrls;
      osuper.uutf.overrideAttrs (old: {
        src = self.fetchurl {
          urls = expectedUrls;
          sha256 = old.src.outputHash;
        };
      });
  });

  # Apply security update for CVE-2018-25032 via overlay instead of waiting for channels.
  zlib =
    let
      patchedVersion = "1.2.12";
    in
    assert self.lib.versionOlder super.zlib.version patchedVersion;
    super.zlib.overrideAttrs (old: {
      version = patchedVersion;
      src = self.fetchurl {
        url = "https://www.zlib.net/fossils/zlib-${patchedVersion}.tar.gz";
        sha256 = "91844808532e5ce316b3c010929493c0244f3d37593afd6de04f71821d5136d9";
      };
      patches = [];
    });
}
