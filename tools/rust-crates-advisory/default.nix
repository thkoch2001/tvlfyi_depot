{ depot, pkgs, lib, ... }:

let

  bins =
    depot.nix.getBins pkgs.s6-portable-utils [ "s6-ln" "s6-cat" "s6-echo" "s6-mkdir" "s6-test" "s6-touch" "s6-dirname" ]
    // depot.nix.getBins pkgs.lr [ "lr" ]
    // depot.nix.getBins pkgs.cargo-audit [ "cargo-audit" ]
    // depot.nix.getBins pkgs.jq [ "jq" ]
    // depot.nix.getBins pkgs.findutils [ "find" ]
    // depot.nix.getBins pkgs.gnused [ "sed" ]
  ;

  crate-advisories = "${depot.third_party.rustsec-advisory-db}/crates";

  our-crates = lib.filter (v: v ? outPath)
    (builtins.attrValues depot.third_party.rust-crates);

  check-security-advisory = depot.nix.writers.rustSimple
    {
      name = "parse-security-advisory";
      dependencies = [
        depot.third_party.rust-crates.toml
        depot.third_party.rust-crates.semver
      ];
    }
    (builtins.readFile ./check-security-advisory.rs);

  # $1 is the directory with advisories for crate $2 with version $3
  check-crate-advisory = depot.nix.writeExecline "check-crate-advisory" { readNArgs = 3; } [
    "pipeline"
    [ bins.lr "-0" "-t" "depth == 1" "$1" ]
    "forstdin"
    "-0"
    "-Eo"
    "0"
    "advisory"
    "if"
    [ depot.tools.eprintf "advisory %s\n" "$advisory" ]
    check-security-advisory
    "$advisory"
    "$3"
  ];

  # Run through everything in the `crate-advisories` repository
  # and check whether we can parse all the advisories without crashing.
  test-parsing-all-security-advisories = depot.nix.runExecline "check-all-our-crates" { } [
    "pipeline"
    [ bins.lr "-0" "-t" "depth == 1" crate-advisories ]
    "if"
    [
      # this will succeed as long as check-crate-advisory doesn’t `panic!()` (status 101)
      "forstdin"
      "-0"
      "-E"
      "-x"
      "101"
      "crate_advisories"
      check-crate-advisory
      "$crate_advisories"
      "foo"
      "0.0.0"
    ]
    "importas"
    "out"
    "out"
    bins.s6-touch
    "$out"
  ];


  check-all-our-crates = depot.nix.runExecline "check-all-our-crates"
    {
      stdin = lib.concatStrings
        (map
          (crate:
            depot.nix.netstring.fromString
              (depot.nix.netstring.fromString crate.crateName
                + depot.nix.netstring.fromString crate.version))
          our-crates);
    } [
    "if"
    [
      "forstdin"
      "-o"
      "0"
      "-Ed"
      ""
      "crateNetstring"
      "multidefine"
      "-d"
      ""
      "$crateNetstring"
      [ "crate" "crate_version" ]
      "if"
      [ depot.tools.eprintf "checking %s, version %s\n" "$crate" "$crate_version" ]

      "ifthenelse"
      [ bins.s6-test "-d" "${crate-advisories}/\${crate}" ]
      [
        # also print the full advisory text if it matches
        "export"
        "PRINT_ADVISORY"
        "1"
        check-crate-advisory
        "${crate-advisories}/\${crate}"
        "$crate"
        "$crate_version"
      ]
      [ depot.tools.eprintf "No advisories found for crate %s\n" "$crate" ]
      "importas"
      "-ui"
      "ret"
      "?"
      # put a marker in ./failed to read at the end
      "ifelse"
      [ bins.s6-test "$ret" "-eq" "1" ]
      [ bins.s6-touch "./failed" ]
      "if"
      [ depot.tools.eprintf "\n" ]
      "exit"
      "$ret"
    ]
    "ifelse"
    [ bins.s6-test "-f" "./failed" ]
    [
      "if"
      [ depot.tools.eprintf "Error: Found active advisories!" ]
      "exit"
      "1"
    ]
    "importas"
    "out"
    "out"
    bins.s6-touch
    "$out"
  ];

  check-all-our-lock-files = depot.nix.writeExecline "check-all-our-lock-files" { } [
    "backtick"
    "-E"
    "report"
    [
      "pipeline"
      [ bins.find "." "-name" "Cargo.lock" "-and" "-type" "f" "-print0" ]
      "forstdin"
      "-E"
      "-0"
      "lockFile"
      "backtick"
      "-E"
      "depotPath"
      [
        "pipeline"
        [ bins.s6-dirname "$lockFile" ]
        bins.sed
        "s|^\\.|/|"
      ]
      "pipeline"
      [
        bins.cargo-audit
        "audit"
        "--json"
        "-n"
        "--db"
        depot.third_party.rustsec-advisory-db
        "-f"
        "$lockFile"
      ]
      bins.jq
      "-rj"
      "--arg"
      "attr"
      "$depotPath"
      "--arg"
      "maintainers"
      ""
      "--argjson"
      "checklist"
      "false"
      "-f"
      ./format-audit-result.jq
    ]
    "if"
    [ depot.tools.eprintf "%s\n" "$report" ]
    "ifelse"
    [ bins.s6-test "-z" "$report" ]
    # empty report implies success (no advisories)
    [ "exit" "0" ]
    # If we reach this point, we know that the report is non-empty, so we should
    # only continue without one if we are running in buildkite.
    "if"
    [
      "importas"
      "-D"
      ""
      "BUILDKITE_BUILD_ID"
      "BUILDKITE_BUILD_ID"
      bins.s6-test
      "-n"
      "$BUILDKITE_BUILD_ID"
    ]
    # If we're running in buildkite, annotate the pipeline run with the report
    # as a warning. Only fail if something goes wrong with buildkite-agent
    # which is assumed to be in PATH.
    "pipeline"
    [
      "printf"
      "%s"
      "$report"
    ]
    "buildkite-agent"
    "annotate"
    "--style"
    "warning"
    "--context"
    "check-all-our-lock-files"
  ];

in
depot.nix.readTree.drvTargets {

  check-all-our-crates =
    depot.nix.drvSeqL
      [ test-parsing-all-security-advisories ]
      check-all-our-crates;

  inherit
    check-crate-advisory
    ;


  check-all-our-lock-files = check-all-our-lock-files // {
    meta.ci.extraSteps.run = {
      label = "Check Cargo.lock files in depot for advisories";
      alwaysRun = true;
      command = check-all-our-lock-files;
    };
  };
}
