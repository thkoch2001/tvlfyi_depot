# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ depot, ... }:

self: super: {
  # Rollback Nix to a stable version (2.3) while there is lots of
  # random ecosystem breakage with the newer versions.
  nix = super.nix_2_3;

  # Required for apereo-cas
  # TODO(lukegb): Document why?
  gradle_6 = self.callPackage (super.gradleGen {
    version = "6.5.1";
    nativeVersion = "0.22-milestone-3";
    sha256 = "0jmmipjh4fbsn92zpifa5cqg5ws2a4ha0s4jzqhrg4zs542x79sh";
  }) {
    java = self.jdk11;
  };

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
      exwm = esuper.exwm.overrideAttrs(_: {
        src = depot.path.origSrc + "/third_party/exwm";
      });
    })
  );

  # dottime support for notmuch
  notmuch = super.notmuch.overrideAttrs(old: {
    passthru = old.passthru // {
      patches = old.patches ++ [ ./patches/notmuch-dottime.patch ];
    };
  });

  # Fix Steam issues with web views (nixpkgs#137279)
  steam = super.steam.override {
    extraPkgs = pkgs: with pkgs; [ pango harfbuzz libthai ];
  };

  # nix-serve does not work with nix 2.4
  # https://github.com/edolstra/nix-serve/issues/28
  nix-serve = super.nix-serve.override { nix = super.nix_2_3; };

  # Avoid builds of mkShell derivations in CI.
  mkShell = super.lib.makeOverridable(args: (super.mkShell args) // {
    meta.ci = false;
  });
}
