{ depot, pkgs, lib, ... }:

let
  # dependency imports

  inherit (depot.nix) getBins;
  inherit (depot.third_party) rustsec-advisory-db;

  bins = getBins pkgs.jq [
    "jq"
  ] // getBins pkgs.coreutils [
    "cat"
    "printf"
    "tee"
    "test"
    "wc"
  ] // getBins pkgs.gnugrep [
    "grep"
  ] // getBins pkgs.cargo-audit [
    "cargo-audit"
  ] // getBins pkgs.ansi2html [
    "ansi2html"
  ] // {
    eprintf = depot.tools.eprintf;
  };

  # list of maintainers we may @mention on GitHub
  maintainerWhitelist = builtins.attrValues {
    inherit (lib.maintainers)
      sternenseemann
      qyliss
      jk
      symphorien
      erictapen
      expipiplus1
      ;
  };

  # buildRustPackage handling

  /* Predicate by which we identify rust packages we are interested in,
     i. e. built using `buildRustPackage`.

     Type :: drv -> bool
  */
  isRustPackage = v: v ? cargoDeps;

  /* Takes a buildRustPackage derivation and returns a derivation which
     builds extracts the `Cargo.lock` of its `cargoDeps` derivation or
     `null` if it has none.

     Type: drv -> option<drv>
  */
  # TODO(sterni): support cargoVendorDir?
  extractCargoLock = drv:
    if !(drv ? cargoDeps.outPath)
    then null
    else
      pkgs.runCommandNoCC "${drv.name}-Cargo.lock" { } ''
        if test -d "${drv.cargoDeps}"; then
          cp "${drv.cargoDeps}/Cargo.lock" "$out"
        fi

        if test -f "${drv.cargoDeps}"; then
          tar -xO \
            --no-wildcards-match-slash --wildcards \
            -f "${drv.cargoDeps}" \
            '*/Cargo.lock' \
            > "$out"
        fi
      '';

  # nixpkgs traversal

  # Condition for us to recurse: Either at top-level or recurseForDerivation.
  recurseInto = path: x: path == [ ] ||
    (lib.isAttrs x && (x.recurseForDerivations or false));

  # Returns the value or false if an eval error occurs.
  tryEvalOrFalse = v: (builtins.tryEval v).value;

  /* Traverses nixpkgs as instructed by `recurseInto` and collects
     the attribute and lockfile derivation of every rust package it
     encounters into a list.

     Type :: attrs
          -> list {
               attr :: list<str>;
               lock :: option<drv>;
               maintainers :: list<maintainer>;
             }
  */
  allLockFiles =
    let
      go = path: x:
        let
          isDrv = tryEvalOrFalse (lib.isDerivation x);
          doRec = tryEvalOrFalse (recurseInto path x);
          isRust = tryEvalOrFalse (isRustPackage x);
        in
        if doRec then
          lib.concatLists
            (
              lib.mapAttrsToList (n: go (path ++ [ n ])) x
            ) else if isDrv && isRust then [
          {
            attr = path;
            lock = extractCargoLock x;
            maintainers = x.meta.maintainers or [ ];
          }
        ] else [ ];
    in
    go [ ];

  # Report generation and formatting

  reportFor = { attr, lock, maintainers ? [ ] }:
    let
      # naïve attribute path to Nix syntax conversion
      strAttr = lib.concatStringsSep "." attr;
      strMaintainers = lib.concatMapStringsSep " " (m: "@${m.github}") (
        builtins.filter (x: builtins.elem x maintainerWhitelist) maintainers
      );
    in
    if lock == null
    then pkgs.emptyFile
    else
      depot.nix.runExecline "${strAttr}-vulnerability-report" { } [
        "pipeline"
        [
          bins.cargo-audit
          "audit"
          "--json"
          "-n"
          "--db"
          rustsec-advisory-db
          "-f"
          lock
        ]
        "importas"
        "out"
        "out"
        "redirfd"
        "-w"
        "1"
        "$out"
        bins.jq
        "-rj"
        "-f"
        ../../../tools/rust-crates-advisory/format-audit-result.jq
        "--arg"
        "attr"
        strAttr
        "--arg"
        "maintainers"
        strMaintainers
      ];

  # GHMF in issues splits paragraphs on newlines
  description = lib.concatMapStringsSep "\n\n"
    (
      builtins.replaceStrings [ "\n" ] [ " " ]
    ) [
    ''
      The vulnerability report below was generated by
      [nixpkgs-crate-holes](https://code.tvl.fyi/tree/users/sterni/nixpkgs-crate-holes)
      which extracts the `Cargo.lock` file of each package in nixpkgs with a
      `cargoDeps` attribute and passes it to
      [cargo-audit](https://github.com/RustSec/rustsec/tree/main/cargo-audit)
      using RustSec's
      [advisory-db at ${builtins.substring 0 7 rustsec-advisory-db.rev}](https://github.com/RustSec/advisory-db/tree/${rustsec-advisory-db.rev}/).
    ''
    ''
      Feel free to report any problems or suggest improvements (I have an email
      address on my profile and hang out on Matrix/libera.chat as sterni)!
      Tick off any reports that have been fixed in the meantime.
    ''
    ''
      Note: A vulnerability in a dependency does not necessarily mean the dependent
      package is vulnerable, e. g. when a vulnerable function isn't used.
    ''
  ];

  runInstructions = ''
    <details>
    <summary>
    Generating Cargo.lock vulnerability reports

    </summary>

    If you have a checkout of [depot](https://code.tvl.fyi/about/), you can generate this report using:

    ```
    nix-build -A users.sterni.nixpkgs-crate-holes.full \
      --argstr nixpkgsPath /path/to/nixpkgs
    ```

    If you want a more detailed report for a single attribute of nixpkgs, use:

    ```
    nix-build -A users.sterni.nixpkgs-crate-holes.single \
      --argstr nixpkgsPath /path/to/nixpkgs --arg attr '[ "ripgrep" ]'
    ```

    </details>
  '';

  defaultNixpkgsArgs = { allowBroken = false; };

  reportForNixpkgs =
    { nixpkgsPath
    , nixpkgsArgs ? defaultNixpkgsArgs
    }@args:

    let
      reports = builtins.map reportFor (
        allLockFiles (import nixpkgsPath nixpkgsArgs)
      );
    in

    depot.nix.runExecline "nixpkgs-rust-pkgs-vulnerability-report.md"
      {
        stdin = lib.concatMapStrings (report: "${report}\n") reports;
      } [
      "importas"
      "out"
      "out"
      "redirfd"
      "-w"
      "1"
      "$out"
      # Print introduction paragraph for the issue
      "if"
      [ bins.printf "%s\n\n" description ]
      # Print all reports
      "foreground"
      [
        "forstdin"
        "-E"
        "report"
        bins.cat
        "$report"
      ]
      # Print stats at the end (mostly as a gimmick), we already know how many
      # attributes there are and count the attributes with vulnerability by
      # finding the number of checkable list entries in the output.
      "backtick"
      "-E"
      "vulnerableCount"
      [
        "pipeline"
        [
          bins.grep
          "^- \\[ \\]"
          "$out"
        ]
        bins.wc
        "-l"
      ]
      "if"
      [
        bins.printf
        "\n%s of %s checked attributes have vulnerable dependencies.\n\n"
        "$vulnerableCount"
        (toString (builtins.length reports))
      ]
      "if"
      [
        bins.printf
        "%s\n\n"
        runInstructions
      ]
    ];

  singleReport =
    {
      # Attribute to check: string or list of strings (attr path)
      attr
      # Path to importable nixpkgs checkout
    , nixpkgsPath
      # Arguments to pass to nixpkgs
    , nixpkgsArgs ? defaultNixpkgsArgs
    }:

    let
      attr' = if builtins.isString attr then [ attr ] else attr;
      drv = lib.getAttrFromPath attr' (import nixpkgsPath nixpkgsArgs);
      lockFile = extractCargoLock drv;
      strAttr = lib.concatStringsSep "." attr';
    in

    depot.nix.runExecline "${strAttr}-report.html" { } [
      "importas"
      "out"
      "out"
      "backtick"
      "-I"
      "-E"
      "-N"
      "report"
      [
        bins.cargo-audit
        "audit"
        "--quiet"
        "-n"
        "--db"
        rustsec-advisory-db
        "-f"
        lockFile
      ]
      "pipeline"
      [
        "ifte"
        [
          bins.printf
          "%s"
          "$report"
        ]
        [
          bins.printf
          "%s\n"
          "No vulnerabilities found"
        ]
        bins.test
        "-n"
        "$report"
      ]
      "pipeline"
      [
        bins.tee
        "/dev/stderr"
      ]
      "redirfd"
      "-w"
      "1"
      "$out"
      bins.ansi2html
    ];

in
{
  full = reportForNixpkgs;
  single = singleReport;

  inherit
    extractCargoLock
    allLockFiles
    ;

  # simple sanity check, doesn't cover everything, but testing the full report
  # is quite expensive in terms of evaluation.
  testSingle = singleReport {
    nixpkgsPath = depot.third_party.nixpkgs.path;
    attr = [ "ripgrep" ];
  };

  meta.targets = [ "testSingle" ];
}
