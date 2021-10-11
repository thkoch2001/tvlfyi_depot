{ depot, pkgs, lib, ... }:

let

  bins =
       depot.nix.getBins pkgs.s6-portable-utils [ "s6-ln" "s6-cat" "s6-echo" "s6-mkdir" "s6-test" "s6-touch" ]
    // depot.nix.getBins pkgs.lr [ "lr" ]
    // depot.nix.getBins pkgs.cargo-audit [ "cargo-audit" ]
    // depot.nix.getBins pkgs.jq [ "jq" ]
    ;

  crate-advisories = "${depot.third_party.rustsec-advisory-db}/crates";

  our-crates = lib.filter (v: v ? outPath)
    (builtins.attrValues depot.third_party.rust-crates);

  check-security-advisory = depot.nix.writers.rustSimple {
    name = "parse-security-advisory";
    dependencies = [
      depot.third_party.rust-crates.toml
      depot.third_party.rust-crates.semver
    ];
  } (builtins.readFile ./check-security-advisory.rs);

  # $1 is the directory with advisories for crate $2 with version $3
  check-crate-advisory = depot.nix.writeExecline "check-crate-advisory" { readNArgs = 3; } [
    "pipeline" [ bins.lr "-0" "-t" "depth == 1" "$1" ]
    "forstdin" "-0" "-Eo" "0" "advisory"
    "if" [ depot.tools.eprintf "advisory %s\n" "$advisory" ]
    check-security-advisory "$advisory" "$3"
  ];

  # Run through everything in the `crate-advisories` repository
  # and check whether we can parse all the advisories without crashing.
  test-parsing-all-security-advisories = depot.nix.runExecline "check-all-our-crates" {} [
    "pipeline" [ bins.lr "-0" "-t" "depth == 1" crate-advisories ]
    "if" [
      # this will succeed as long as check-crate-advisory doesn’t `panic!()` (status 101)
      "forstdin" "-0" "-E" "-x" "101" "crate_advisories"
      check-crate-advisory "$crate_advisories" "foo" "0.0.0"
    ]
    "importas" "out" "out"
    bins.s6-touch "$out"
  ];


  check-all-our-crates = depot.nix.runExecline "check-all-our-crates" {
    stdin = lib.concatStrings
      (map
        (crate:
          depot.nix.netstring.fromString
            ( depot.nix.netstring.fromString crate.crateName
            + depot.nix.netstring.fromString crate.version ))
        our-crates);
  } [
    "if" [
      "forstdin" "-o" "0" "-Ed" "" "crateNetstring"
      "multidefine" "-d" "" "$crateNetstring" [ "crate" "crate_version" ]
      "if" [ depot.tools.eprintf "checking %s, version %s\n" "$crate" "$crate_version" ]

      "ifthenelse" [ bins.s6-test "-d" "${crate-advisories}/\${crate}" ]
          [ # also print the full advisory text if it matches
            "export" "PRINT_ADVISORY" "1"
            check-crate-advisory "${crate-advisories}/\${crate}" "$crate" "$crate_version"
          ]
        [ depot.tools.eprintf "No advisories found for crate %s\n" "$crate" ]
        "importas" "-ui" "ret" "?"
        # put a marker in ./failed to read at the end
        "ifelse" [ bins.s6-test "$ret" "-eq" "1" ]
          [ bins.s6-touch "./failed" ]
        "if" [ depot.tools.eprintf "\n" ]
        "exit" "$ret"
    ]
    "ifelse" [ bins.s6-test "-f" "./failed" ]
      [ "if" [ depot.tools.eprintf "Error: Found active advisories!" ]
        "exit" "1"
      ]
    "importas" "out" "out"
    bins.s6-touch "$out"
  ];

  # Find all `Cargo.lock` files in the given path which must be a directory
  allCargoLocks = path: let
    dir = builtins.readDir path;
    subdirs = builtins.map (sub: path + "/${sub}") (
      builtins.attrNames (
        lib.filterAttrs (_: v: v == "directory") dir
      )
    );
  in
    lib.optional (dir."Cargo.lock" or null == "regular") (path + "/Cargo.lock")
    ++ lib.concatLists (
      builtins.map allCargoLocks subdirs
    );

  # Converts a nix path below `depot.path.origSrc` into a string path relative
  # to the depot root indicated using `//`.
  toDepotPath = path:
    builtins.replaceStrings [ (toString depot.path.origSrc) ] [ "/" ] (toString path);

  check-all-our-lock-files = depot.nix.runExecline "check-all-our-lock-files" {
    stdin = lib.concatMapStrings (path:
      depot.nix.netstring.fromString "${path}"
      + depot.nix.netstring.fromString (toDepotPath path)
      + "\n"
    ) (allCargoLocks depot.path.origSrc);
  } [
    "backtick" "-E" "report" [
      "forstdin" "-E" "-n" "lockFileData"
      "multidefine" "-d" "" "$lockFileData" [ "lockFile" "depotPath" ]
      "pipeline" [
        bins.cargo-audit "audit" "--json"
        "-n" "--db" depot.third_party.rustsec-advisory-db
        "-f" "$lockFile"
      ]
      bins.jq "-rj" "--arg" "attr" "$depotPath"
      "-f" ../../users/sterni/nixpkgs-crate-holes/format-audit-result.jq
    ]
    "if" [ depot.tools.eprintf "%s\n" "$report" ]
    # empty report implies success (no vulnerabilities)
    "if" [ bins.s6-test "-z" "$report" ]
    "importas" "out" "out"
    bins.s6-touch "$out"
  ];

in depot.nix.utils.drvTargets {

  check-all-our-crates =
    depot.nix.drvSeqL
      [ test-parsing-all-security-advisories ]
      check-all-our-crates;

  inherit
    check-all-our-lock-files
    check-crate-advisory
    ;
}
