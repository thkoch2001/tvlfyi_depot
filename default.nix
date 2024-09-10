<<<<<<< HEAD   (e573f8 docs(tvix/nix-compat/wire/bytes/reader): None case doesn't e)
# This file sets up the top-level package set by traversing the package tree
# (see //nix/readTree for details) and constructing a matching attribute set
# tree.

{ nixpkgsBisectPath ? null
, parentTargetMap ? null
, nixpkgsConfig ? { }
, localSystem ? builtins.currentSystem
, crossSystem ? null
, ...
}@args:

let
  inherit (builtins)
    filter
    ;

  readTree = import ./nix/readTree { };

  # Disallow access to //users from other depot parts.
  usersFilter = readTree.restrictFolder {
    folder = "users";
    reason = ''
      Code under //users is not considered stable or dependable in the
      wider depot context. If a project under //users is required by
      something else, please move it to a different depot path.
    '';

    exceptions = [
      # whitby is allowed to access //users for several reasons:
      #
      # 1. User SSH keys are set in //users.
      # 2. Some personal websites or demo projects are served from it.
      [ "ops" "machines" "whitby" ]

      # Due to evaluation order this also affects these targets.
      # TODO(tazjin): Can this one be removed somehow?
      [ "ops" "nixos" ]
      [ "ops" "machines" "all-systems" ]
    ];
  };

  # Disallow access to //corp from other depot parts.
  corpFilter = readTree.restrictFolder {
    folder = "corp";
    reason = ''
      Code under //corp may use incompatible licensing terms with
      other depot parts and should not be used anywhere else.
    '';

    exceptions = [
      # For the same reason as above, whitby is exempt to serve the
      # corp website.
      [ "ops" "machines" "whitby" ]
      [ "ops" "nixos" ]
      [ "ops" "machines" "all-systems" ]
    ];
  };

  readDepot = depotArgs: readTree {
    args = depotArgs;
    path = ./.;
    filter = parts: args: corpFilter parts (usersFilter parts args);
    scopedArgs = {
      __findFile = _: _: throw "Do not import from NIX_PATH in the depot!";
      builtins = builtins // {
        currentSystem = throw "Use localSystem from the readTree args instead of builtins.currentSystem!";
      };
    };
  };

  # To determine build targets, we walk through the depot tree and
  # fetch attributes that were imported by readTree and are buildable.
  #
  # Any build target that contains `meta.ci.skip = true` or is marked
  # broken will be skipped.
  # Is this tree node eligible for build inclusion?
  eligible = node: (node ? outPath) && !(node.meta.ci.skip or (node.meta.broken or false));

in
readTree.fix (self: (readDepot {
  inherit localSystem crossSystem;
  depot = self;

  # Pass third_party as 'pkgs' (for compatibility with external
  # imports for certain subdirectories)
  pkgs = self.third_party.nixpkgs;

  # Expose lib attribute to packages.
  lib = self.third_party.nixpkgs.lib;

  # Pass arguments passed to the entire depot through, for packages
  # that would like to add functionality based on this.
  #
  # Note that it is intended for exceptional circumstance, such as
  # debugging by bisecting nixpkgs.
  externalArgs = args;
}) // {
  # Make the path to the depot available for things that might need it
  # (e.g. NixOS module inclusions)
  path = self.third_party.nixpkgs.lib.cleanSourceWith {
    name = "depot";
    src = ./.;
    filter = self.third_party.nixpkgs.lib.cleanSourceFilter;
  };

  # Additionally targets can be excluded from CI by adding them to the
  # list below.
  ci.excluded = [
    # xanthous and related targets are disabled until cl/9186 is submitted
    self.users.aspen.xanthous
    self.users.aspen.system.system.mugwumpSystem

    # Temporarily disabled after cl/11289. Hopefully these failures are transient
    # and will disappear with the next channel bump.
    self.users.wpcarro.nixos.avaSystem
    self.users.wpcarro.nixos.kyokoSystem
    self.users.wpcarro.nixos.marcusSystem
    self.users.wpcarro.nixos.tarascoSystem
  ];

  # List of all buildable targets, for CI purposes.
  #
  # Note: To prevent infinite recursion, this *must* be a nested
  # attribute set (which does not have a __readTree attribute).
  ci.targets = readTree.gather
    (t: (eligible t) && (!builtins.elem t self.ci.excluded))
    (self // {
      # remove the pipelines themselves from the set over which to
      # generate pipelines because that also leads to infinite
      # recursion.
      ops = self.ops // { pipelines = null; };
    });

  # Derivation that gcroots all depot targets.
  ci.gcroot = with self.third_party.nixpkgs; writeText "depot-gcroot"
    (builtins.concatStringsSep "\n"
      (lib.flatten
        (map (p: map (o: p.${o}) p.outputs or [ ]) # list all outputs of each drv
          self.ci.targets)));
})
=======
{
  pkgs ? import <nixpkgs> { },
  lib ? pkgs.lib,
  ...
}:

let
  buildNimSbom = pkgs.callPackage ./build-nim-sbom.nix { };
  nix' = pkgs.nixVersions.latest.overrideAttrs (_: {
    version = "2024-08-23";
    src = pkgs.fetchFromGitHub {
      owner = "nixos";
      repo = "nix";
      rev = "85f1aa6b3df5c5fcc924a74e2a9cc8acea9ba0e1";
      hash = "sha256-3+UgAktTtkGUNpxMxr+q+R+z3r026L3PwJzG6RD2IXM=";
    };
  });
in
buildNimSbom (finalAttrs: {
  outputs = [
    "out"
    "cfg"
  ];
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = [ nix' ];
  src = if lib.inNixShell then null else lib.cleanSource ./.;
  postInstall = ''
    mkdir $cfg
    export mainProgram="$out/bin/nix-actor"
    substituteAll service.pr.in $cfg/service.pr
  '';
}) ./sbom.json
>>>>>>> BRANCH (d8606c Make defaut.nix TVL depot compatible)
