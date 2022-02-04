{ depot, pkgs, lib, ... }:

let

  bins =
    depot.nix.getBins pkgs.s6-portable-utils [ "s6-ln" "s6-cat" "s6-echo" "s6-mkdir" "s6-test" "s6-touch" "s6-dirname" ]
    // depot.nix.getBins pkgs.coreutils [ "printf" ]
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

  lock-file-report = pkgs.writers.writeBash "lock-file-report" ''
    set -u

    if test "$#" -lt 2; then
      echo "Usage: $0 IDENTIFIER LOCKFILE [CHECKLIST [MAINTAINERS]]" >&2
      echo 2>&1
      echo "  IDENTIFIER  Unique string describing the lock file" >&2
      echo "  LOCKFILE    Path to Cargo.lock file" >&2
      echo "  CHECKLIST   Whether to use GHFM checklists in the output (true or false)" >&2
      echo "  MAINTAINERS List of @names to cc in case of advisories" >&2
      exit 100
    fi

    "${bins.cargo-audit}" audit --json -n \
      --db "${depot.third_party.rustsec-advisory-db}" \
      -f "$2" \
    | "${bins.jq}" -rj -f "${./format-audit-result.jq}" \
      --arg maintainers "''${4:-}" --argjson checklist "''${3:-false}" \
      --arg attr "$1"

    exit "''${PIPESTATUS[0]}" # inherit exit code from cargo-audit
  '';

  tree-lock-file-report = depot.nix.writeExecline "tree-lock-file-report"
    {
      readNArgs = 1;
    } [
    "backtick"
    "-E"
    "report"
    [
      "pipeline"
      [ bins.find "$1" "-name" "Cargo.lock" "-and" "-type" "f" "-print0" ]
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
      lock-file-report
      "$depotPath"
      "$lockFile"
      "false"
    ]
    "if"
    [ bins.printf "%s\n" "$report" ]
    # empty report implies success (no advisories)
    bins.s6-test
    "-z"
    "$report"
  ];

  check-all-our-lock-files = depot.nix.writeExecline "check-all-our-lock-files" { } [
    "backtick"
    "-EI"
    "report"
    [
      tree-lock-file-report
      "."
    ]
    "ifelse"
    [
      bins.s6-test
      "-z"
      "$report"
    ]
    [
      "exit"
      "0"
    ]
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
    lock-file-report
    ;


  tree-lock-file-report = tree-lock-file-report // {
    meta.ci.extraSteps.run = {
      label = "Check Cargo.lock files in depot for advisories";
      alwaysRun = true;
      command = check-all-our-lock-files;
    };
  };
}
