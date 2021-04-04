{ depot, pkgs, lib, ... }:

let

  bins =
       depot.nix.getBins pkgs.s6-portable-utils [ "s6-ln" "s6-cat" "s6-echo" "s6-mkdir" "s6-test" "s6-touch" ]
    // depot.nix.getBins pkgs.lr [ "lr" ]
    ;

  crate-advisories = "${pkgs.fetchFromGitHub {
    owner = "RustSec";
    repo = "advisory-db";
    # TODO(Profpatsch): this will have to be updated regularly, how?
    rev = "113188c62380753f01ff0df5edb7d67a300b143a";
    sha256 = "0v086ybwr71zgs5nv8yr4w2w2d4daxx6in2s1sjb4m41q1r9p0wj";
  }}/crates";

  our-crates = lib.mapAttrsToList (_: lib.id)
    # this is a bit eh, but no idea how to avoid the readTree thing otherwise
    (builtins.removeAttrs depot.third_party.rust-crates [ "__readTree" ]);

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

in depot.nix.utils.drvTargets {

  check-all-our-crates =
    depot.nix.drvSeqL
      [ test-parsing-all-security-advisories ]
      check-all-our-crates;

  inherit
    check-crate-advisory
    ;
}
