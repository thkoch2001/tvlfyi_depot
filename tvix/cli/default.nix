{ depot, pkgs, lib, ... }:

(depot.tvix.crates.workspaceMembers.tvix-cli.build.override {
  runTests = true;
  testPreRun = ''
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt;
  '';
}).overrideAttrs (finalAttrs: previousAttrs:

let
  tvix-cli = finalAttrs.finalPackage;

  benchmark-gnutime-format-string =
    description:
    "Benchmark: " +
    (builtins.toJSON {
      "${description}" = {
        kbytes = "%M";
        system = "%S";
        user = "%U";
      };
    });

  # You can run the benchmark with a simple `nix run`, like:
  #
  #  nix-build -A tvix.cli.meta.ci.extraSteps.benchmark-nixpkgs-cross-hello-outpath
  #
  # TODO(amjoseph): store these results someplace more durable, like git trailers
  #
  mkExprBenchmark = { expr, description }:
    let name = "tvix-cli-benchmark-${description}"; in
    (pkgs.runCommand name { } ''
      export SSL_CERT_FILE=${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt
      ${lib.escapeShellArgs [
        "${pkgs.time}/bin/time"
        "--format" "${benchmark-gnutime-format-string description}"
        "${tvix-cli}/bin/tvix"
        "--no-warnings"
        "-E" expr
      ]}
      touch $out
    '');

  mkNixpkgsBenchmark = attrpath:
    mkExprBenchmark {
      description = builtins.replaceStrings [ ".drv" ] [ "-drv" ] attrpath;
      expr = "(import ${pkgs.path} {}).${attrpath}";
    };

  # Constructs a Derivation invoking tvix-cli inside a build, ensures the
  # calculated tvix output path matches what's passed in externally.
  mkNixpkgsEvalTest = attrpath: expectedPath:
    let
      name = "tvix-eval-test-${builtins.replaceStrings [".drv"] ["-drv"] attrpath}";
    in
    (pkgs.runCommand name { } ''
      export SSL_CERT_FILE=${pkgs.cacert.out}/etc/ssl/certs/ca-bundle.crt
      TVIX_OUTPUT=$(${tvix-cli}/bin/tvix -E '(import ${pkgs.path} {}).${attrpath}')
      EXPECTED='${/* the verbatim expected Tvix output: */ "=> \"${builtins.unsafeDiscardStringContext expectedPath}\" :: string"}'

      echo "Tvix output: ''${TVIX_OUTPUT}"
      if [ "$TVIX_OUTPUT" != "$EXPECTED" ]; then
        echo "Correct would have been ''${EXPECTED}"
        exit 1
      fi

      echo "Output was correct."
      touch $out
    '');


  benchmarks = {
    benchmark-hello = (mkNixpkgsBenchmark "hello.outPath");
    benchmark-cross-hello = (mkNixpkgsBenchmark "pkgsCross.aarch64-multiplatform.hello.outPath");
    benchmark-firefox = (mkNixpkgsBenchmark "firefox.outPath");
    benchmark-cross-firefox = (mkNixpkgsBenchmark "pkgsCross.aarch64-multiplatform.firefox.outPath");
    # Example used for benchmarking LightSpan::Delayed in commit bf286a54bc2ac5eeb78c3d5c5ae66e9af24d74d4
    benchmark-nixpkgs-attrnames = (mkExprBenchmark { expr = "builtins.length (builtins.attrNames (import ${pkgs.path} {}))"; description = "nixpkgs-attrnames"; });
  };

  evalTests = {
    eval-nixpkgs-stdenv-drvpath = (mkNixpkgsEvalTest "stdenv.drvPath" pkgs.stdenv.drvPath);
    eval-nixpkgs-stdenv-outpath = (mkNixpkgsEvalTest "stdenv.outPath" pkgs.stdenv.outPath);
    eval-nixpkgs-hello-outpath = (mkNixpkgsEvalTest "hello.outPath" pkgs.hello.outPath);
    eval-nixpkgs-firefox-outpath = (mkNixpkgsEvalTest "firefox.outPath" pkgs.firefox.outPath);
    eval-nixpkgs-firefox-drvpath = (mkNixpkgsEvalTest "firefox.drvPath" pkgs.firefox.drvPath);
    eval-nixpkgs-cross-stdenv-outpath = (mkNixpkgsEvalTest "pkgsCross.aarch64-multiplatform.stdenv.outPath" pkgs.pkgsCross.aarch64-multiplatform.stdenv.outPath);
    eval-nixpkgs-cross-hello-outpath = (mkNixpkgsEvalTest "pkgsCross.aarch64-multiplatform.hello.outPath" pkgs.pkgsCross.aarch64-multiplatform.hello.outPath);
  };
in
{
  meta = {
    ci.targets = (builtins.attrNames benchmarks) ++ (builtins.attrNames evalTests);
  };

  # Expose benchmarks and evalTests as standard CI targets.
  passthru = benchmarks // evalTests;
})
