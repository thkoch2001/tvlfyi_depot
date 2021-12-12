# Builder for depot-internal Emacs packages. Packages built using this
# builder are added into the Emacs packages fixpoint under
# `emacsPackages.tvlPackages`, which in turn makes it possible to use
# them with special Emacs features like native compilation.
#
# Arguments passed to the builder are the same as
# emacsPackages.trivialBuild, except:
#
# * packageRequires is not used
#
# * externalRequires takes a selection function for packages from
#   emacsPackages
#
# * internalRequires takes other depot packages
{ pkgs, ... }:

buildArgs:

pkgs.callPackage ({ emacsPackages }:

  let
    # Select external dependencies from the emacsPackages set
    externalDeps = (buildArgs.externalRequires or (_: [ ])) emacsPackages;

    # Override emacsPackages for depot-internal packages
    internalDeps = map (p: p.override { inherit emacsPackages; })
      (buildArgs.internalRequires or [ ]);

    trivialBuildArgs =
      builtins.removeAttrs buildArgs [ "externalRequires" "internalRequires" ]
      // {
        packageRequires = externalDeps ++ internalDeps;
      };
  in emacsPackages.trivialBuild trivialBuildArgs) { }
