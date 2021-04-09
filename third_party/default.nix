# This file defines the root of all external dependency imports (i.e.
# third-party code) in the TVL package tree.
#
# There are two categories of third-party programs:
#
# 1) Programs in nixpkgs, the NixOS package set.
# 2) Third-party software packaged in this repository.
#
# In some cases where we locally package something that already exists
# in nixpkgs (for example because we want different versions) the
# overlay definitions below are used to replace the existing package
# in nixpkgs.
#
# Other source locations you should be aware of:
#
#   //third_party/nixpkgs - this imports the nixpkgs package set

{ depot, ... }:

let
  # Tracking nixos-unstable as of 2021-03-25.
  unstableHashes = {
    commit = "60dd94fb7e01a8288f6638eee71d7cb354c49327";
    sha256 = "0skdwk9bdld295kzrymirs8xrzycqmhsclaz8s18jhcz75hb8sk3";
  };

  # Tracking nixos-20.09 as of 2021-03-25.
  stableHashes = {
    commit = "223d0d733a66b46504ea6b4c15f88b7cc4db58fb";
    sha256 = "073327ris0frqa3kpid3nsjr9w8yx2z83xpsc24w898mrs9r7d5v";
  };
in {
  # This provides the sources of nixpkgs. We track both
  # nixos-unstable, and the current stable channel of the latest NixOS
  # release. Please see //third_party/nixpkgs for the actual use of
  # these sources.
  nixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${unstableHashes.commit}.tar.gz";
    sha256 = unstableHashes.sha256;
  };

  stableNixpkgsSrc = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${stableHashes.commit}.tar.gz";
    sha256 = stableHashes.sha256;
  };

  # This overlay is used to make TVL-specific modifications in the
  # nixpkgs tree, where required.
  tvlOverlay = self: super: {
    # Avoid CI traversal into nixpkgs itself
    meta.ci = false;

    # Apply all fixes for the Haskell package set
    haskellPackages = super.haskellPackages.override {
      overrides = depot.third_party.haskellOverlay {
        pkgs = super;
      };
    };

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

    # Use LLVM 11
    llvmPackages = self.llvmPackages_11;
    clangStdenv = self.llvmPackages_11.stdenv;
    clang-tools = (super.clang-tools.override {
      llvmPackages = self.llvmPackages_11;
    });
  };
}
