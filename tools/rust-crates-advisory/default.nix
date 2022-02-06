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

  our-crates-lock-file = pkgs.writeText "our-crates-Cargo.lock"
    (lib.concatMapStrings
      (crate: ''
        [[package]]
        name = "${crate.crateName}"
        version = "${crate.version}"
        source = "registry+https://github.com/rust-lang/crates.io-index"

      '')
      our-crates);

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

  tree-lock-file-report = pkgs.writers.writeBash "tree-lock-file-report" ''
    set -euo pipefail
    status=true

    while IFS= read -r -d $'\0' lockFile; do
      depotPath="$(${bins.s6-dirname} "$lockFile" | ${bins.sed} 's|^\.|/|')"

      ${lock-file-report} "$depotPath" "$lockFile" || status=false
    done < <(${bins.find} "''${1:-.}" -name Cargo.lock -and -type f -print0)

    exec $status
  '';

  depot-rust-crates-advisory-report = pkgs.writers.writeBash "depot-advisory-report" ''
    set -eu
    status=true

    ${lock-file-report} "//third_party/rust-crates" "${our-crates-lock-file}" || status=false
    ${tree-lock-file-report} || status=false

    exec $status
  '';

  buidkiteReportStep =
    { command
    , context ? null
    , style ? "warning"
    }:
    pkgs.writers.writeBash "buildkite-report-${builtins.head command}" ''
      set -u

      report="$(${lib.escapeShellArgs command})"

      if test $? -ne 0; then
         buildkite-agent annotate --style "${style}" \
           ${lib.optionalString (context != null) "--context \"${context}\""} \
           "$report"
      fi
    '';

in
depot.nix.readTree.drvTargets {
  inherit
    test-parsing-all-security-advisories
    check-crate-advisory
    lock-file-report
    ;

  tree-lock-file-report = tree-lock-file-report // {
    meta.ci.extraSteps.run = {
      label = "Check all crates used in depot for advisories";
      alwaysRun = true;
      command = buildkiteReportStep {
        command = [ depot-rust-crates-advisory-report ];
        style = "warning";
      };
    };
  };
}
