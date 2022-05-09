# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ depot, ... }:

self: super:
let
  # Rollback Nix to a stable version (2.3) with backports for
  # build-user problems applied.
  nixSrc = self.fetchFromGitHub
    {
      owner = "tvlfyi";
      repo = "nix";
      # branch 2.3-backport-await-users
      rev = "880a62b08443a6baa55dab027b69bb8b1551a588";
      hash = "sha256:0jnwrzxh04d0pyhx4n8fg4w1w6ak48byl5k2i8j7fk4h9vd9649k";
    } // { revCount = 0; shortRev = "880a62b0"; };
in
{
  nix = (import "${nixSrc}/release.nix" {
    nix = nixSrc;
    nixpkgs = super.path;
    systems = [ builtins.currentSystem ];
  }).build."${builtins.currentSystem}";

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

  # upgrade home-manager until the service-generation fix has landed upstream
  # https://github.com/nix-community/home-manager/issues/2846
  home-manager = super.home-manager.overrideAttrs (old: rec {
    version = assert super.home-manager.version == "2021-12-25"; "2022-04-08";
    src = self.fetchFromGitHub {
      owner = "nix-community";
      repo = "home-manager";
      rev = "f911ebbec927e8e9b582f2e32e2b35f730074cfc";
      sha256 = "07qa2qkbjczj3d0m03jpw85hfj35cbjm48xhifz3viy4khjw88vl";
    };
  });

  python38 = super.python38.override {
    packageOverrides = pySelf: pySuper: {
      backports-zoneinfo = pySuper.backports-zoneinfo.overridePythonAttrs (_: {
        # Outdated test-data, see https://github.com/pganssle/zoneinfo/pull/115
        # Can be dropped when https://github.com/NixOS/nixpkgs/pull/170450 lands.
        doCheck = false;
      });
    };
  };
}
