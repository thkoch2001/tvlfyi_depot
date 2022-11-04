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

  our-crates-lock-file = pkgs.writeText "our-crates-Cargo.lock"
    (lib.concatMapStrings
      (crate: ''
        [[package]]
        name = "${crate.crateName}"
        version = "${crate.version}"
        source = "registry+https://github.com/rust-lang/crates.io-index"

      '')
      our-crates);

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

    "${bins.cargo-audit}" audit --json --no-fetch \
      --db "${depot.third_party.rustsec-advisory-db}" \
      --file "$2" \
    | "${bins.jq}" --raw-output --join-output \
      --from-file "${./format-audit-result.jq}" \
      --arg maintainers "''${4:-}" \
      --argjson checklist "''${3:-false}" \
      --arg attr "$1"

    exit "''${PIPESTATUS[0]}" # inherit exit code from cargo-audit
  '';

  tree-lock-file-report = pkgs.writers.writeBash "tree-lock-file-report" ''
    set -euo pipefail
    status=0

    root="''${1:-.}"

    # Find prints the found lockfiles as <DEPOT ROOT>\t<LOCKFILE DIR>\t<LOCKFILE PATH>\0
    while IFS=$'\t' read -r -d $'\0' entryPoint dir lockFile; do
      label="$(printf '%s' "$dir" | "${bins.sed}" "s|^$entryPoint|/|")"
      "${lock-file-report}" "$label" "$lockFile" || status=1
    done < <("${bins.find}" "$root" -type f -name Cargo.lock -printf '%H\t%h\t%p\0' )

    exit $status
  '';

  check-all-our-lock-files = depot.nix.writeExecline "check-all-our-lock-files" { } [
    "backtick"
    "-EI"
    "report"
    [
      "foreground"
      [
        lock-file-report
        "//third_party/rust-crates"
        our-crates-lock-file
        "false"
      ]
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
  inherit
    lock-file-report
    ;

  tree-lock-file-report = tree-lock-file-report // {
    meta.ci.extraSteps.run = {
      label = "Check all crates used in depot for advisories";
      alwaysRun = true;
      command = check-all-our-lock-files;
    };
  };
}
