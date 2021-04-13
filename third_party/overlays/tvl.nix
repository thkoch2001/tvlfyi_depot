# This overlay is used to make TVL-specific modifications in the
# nixpkgs tree, where required.
{ depot, ... }:

self: super: {
  # Required for apereo-cas
  # TODO(lukegb): Document why?
  gradle_6 = (super.gradleGen.override {
    java = self.jdk11;
    jdk = self.jdk11;
  }).gradleGen rec {
    name = "gradle-6.5.1";
    nativeVersion = "0.22-milestone-3";

    src = builtins.fetchurl {
      url = "https://services.gradle.org/distributions/${name}-bin.zip";
      sha256 = "0jmmipjh4fbsn92zpifa5cqg5ws2a4ha0s4jzqhrg4zs542x79sh";
    };
  };

  # Use LLVM 12
  llvmPackages = self.llvmPackages_12;
  clangStdenv = self.llvmPackages_12.stdenv;
  clang-tools = (super.clang-tools.override {
    llvmPackages = self.llvmPackages_12;
  });

  # Add our Emacs packages to the fixpoint
  emacsPackagesFor = emacs: (
    (super.emacsPackagesFor emacs).overrideScope' (eself: esuper: {
      tvlPackages = depot.tools.emacs-pkgs // depot.third_party.emacs;
    })
  );
}
