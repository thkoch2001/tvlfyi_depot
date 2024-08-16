{ lib, depot, pkgs, ... }:

{
  mkFeaturePowerset = { crateName, features, override ? { } }:
    let
      powerset = xs:
        let
          addElement = set: element:
            set ++ map (e: [ element ] ++ e) set;
        in
        lib.foldl' addElement [ [ ] ] xs;
    in
    lib.listToAttrs (map
      (featuresPowerset: {
        name = if featuresPowerset != [ ] then "with-features-${lib.concatStringsSep "-" featuresPowerset}" else "no-features";
        value = depot.tvix.crates.workspaceMembers.${crateName}.build.override (old: {
          runTests = true;
          features = featuresPowerset;
        } // (if lib.isFunction override then override old else override)
        );
      })
      (powerset features));

  # Filters the given source, only keeping files related to the build, preventing unnecessary rebuilds.
  # Includes src in the root, all other .rs files and optionally Cargo specific files.
  # Additional files to be included can be specified in extraFileset.
  filterRustCrateSrc =
    { root # The original src
    , extraFileset ? null # Additional filesets to include (e.g. fileFilter for proto files)
    , cargoSupport ? false
    }:
    lib.fileset.toSource {
      inherit root;
      fileset = lib.fileset.intersection
        (lib.fileset.fromSource root) # We build our final fileset from the original src
        (lib.fileset.unions ([
          (root + "/src")
          (lib.fileset.fileFilter (f: f.hasExt "rs") root)
        ] ++ lib.optionals cargoSupport [
          (lib.fileset.fileFilter (f: f.name == "Cargo.toml") root)
          (lib.fileset.maybeMissing (root + "/Cargo.lock"))
        ] ++ lib.optional (extraFileset != null) extraFileset));
    };

  # A function which takes a pkgs instance and returns an overriden defaultCrateOverrides with support for tvix crates.
  # This can be used throughout the rest of the repo.
  defaultCrateOverridesForPkgs = pkgs:
    let
      commonDarwinDeps = with pkgs.darwin.apple_sdk.frameworks; [
        Security
        SystemConfiguration
      ];
    in
    pkgs.defaultCrateOverrides // {
      nar-bridge = prev: {
        src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
      };

      nix-compat = prev: {
        src = depot.tvix.utils.filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = root + "/testdata";
        };
      };
      tvix-build = prev: {
        src = depot.tvix.utils.filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = lib.fileset.fileFilter (f: f.hasExt "proto") root;
        };
        PROTO_ROOT = depot.tvix.build.protos.protos;
        nativeBuildInputs = [ pkgs.protobuf ];
        buildInputs = lib.optional pkgs.stdenv.isDarwin commonDarwinDeps;
      };

      tvix-castore = prev: {
        src = depot.tvix.utils.filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = lib.fileset.fileFilter (f: f.hasExt "proto") root;
        };
        PROTO_ROOT = depot.tvix.castore.protos.protos;
        nativeBuildInputs = [ pkgs.protobuf ];
      };

      tvix-cli = prev: {
        src = depot.tvix.utils.filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = root + "/tests";
        };
        buildInputs = lib.optional pkgs.stdenv.isDarwin commonDarwinDeps;
      };

      tvix-store = prev: {
        src = depot.tvix.utils.filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = lib.fileset.fileFilter (f: f.hasExt "proto") root;
        };
        PROTO_ROOT = depot.tvix.store.protos.protos;
        nativeBuildInputs = [ pkgs.protobuf ];
        # fuse-backend-rs uses DiskArbitration framework to handle mount/unmount on Darwin
        buildInputs = lib.optional pkgs.stdenv.isDarwin (commonDarwinDeps ++ pkgs.darwin.apple_sdk.frameworks.DiskArbitration);
      };

      tvix-eval-builtin-macros = prev: {
        src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
      };

      tvix-eval = prev: {
        src = depot.tvix.utils.filterRustCrateSrc rec {
          root = prev.src.origSrc;
          extraFileset = root + "/proptest-regressions";
        };
      };

      tvix-glue = prev: {
        src = depot.tvix.utils.filterRustCrateSrc {
          root = prev.src.origSrc;
        };
      };

      tvix-serde = prev: {
        src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
      };

      tvix-tracing = prev: {
        src = depot.tvix.utils.filterRustCrateSrc { root = prev.src.origSrc; };
      };
    };

  # This creates a derivation that checks whether the Cargo.nix file is up-to-date.
  mkCrate2nixCheck =
    { relCrateDir # A string with the path to the directory with Cargo.{toml,lock,nix}, relative to the repo's root. e.g. "web/tvixbolt".
    , workspace ? false # Whether the Cargo project is a workspace with members.
    }:
    lib.throwIfNot (lib.isString relCrateDir) "The relCrateDir argument to mkCrate2nixCheck must be a string."
      pkgs.stdenvNoCC.mkDerivation
      rec {
        # Important: we include the hash of all Cargo related files in the derivation name.
        # This forces the FOD to be rebuilt/re-verified whenever one of them changes.
        name = "crate2nix-check-" + builtins.substring 0 8 (builtins.hashString "sha256"
          (lib.concatMapStrings (f: builtins.hashFile "sha256" f)
            ([ "${src}/${relCrateDir}/Cargo.toml" "${src}/${relCrateDir}/Cargo.lock" ]
              ++ lib.optionals workspace (map (m: "${src}/${relCrateDir}/${m}/Cargo.toml") (lib.importTOML "${src}/${relCrateDir}/Cargo.toml").workspace.members))
          )
        );

        src = lib.fileset.toSource rec {
          root = ../.;
          fileset = (lib.fileset.intersection
            (lib.fileset.fromSource (depot.third_party.gitignoreSource root))
            (lib.fileset.unions (map
              (p:
                (lib.fileset.fileFilter
                  (f:
                    f.name == "Cargo.toml"
                    || f.name == "Cargo.lock"
                    || f.name == "Cargo.nix"
                    || f.name == "crate-hashes.json"
                    || f.hasExt "rs" # Technically we don't need all of the .rs files, but it's less fragile like this.
                  )
                  p)) [
              (root + "/tvix")
              (root + "/${relCrateDir}")
            ])));
        };

        sourceRoot = "${src.name}/${relCrateDir}";

        nativeBuildInputs = with pkgs; [
          cacert
          cargo
        ];

        buildPhase = ''
          export CARGO_HOME=$(mktemp -d)

          # The following command can be omitted, in which case
          # crate2nix-generate will run it automatically, but won't show the
          # output, which makes it look like the build is somehow "stuck" for a
          # minute or two.
          cargo metadata > /dev/null

          # Generate a new Cargo.nix
          ${pkgs.crate2nix}/bin/crate2nix generate --all-features
          # Format it
          ${pkgs.treefmt}/bin/treefmt Cargo.nix \
            --no-cache \
            --on-unmatched=debug \
            --config-file=${depot.tools.depotfmt.config} \
            --tree-root=.


          # Technically unnecessary, but provides more-helpful output in case of error.
          diff -ur Cargo.nix "${src}/${relCrateDir}"

          # The FOD hash will check that the (re-)generated Cargo.nix matches the already existing Cargo.nix.
          cp Cargo.nix $out
        '';

        # This is an FOD in order to allow `cargo` to perform network access.
        outputHashMode = "flat";
        outputHashAlgo = "sha256";
        outputHash = builtins.hashFile "sha256" "${src}/${relCrateDir}/Cargo.nix";
      };
}
