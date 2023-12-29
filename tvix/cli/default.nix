{ depot, pkgs, lib, ... }:

let
  mkNixpkgsEvalCheck = attrset: expectedPath: {
    label = ":nix: evaluate nixpkgs.${attrset} in tvix";
    needsOutput = true;

    command = pkgs.writeShellScript "tvix-eval-${builtins.replaceStrings [".drv"] ["-drv"] attrset}" ''
      TVIX_OUTPUT=$(result/bin/tvix -E '(import ${pkgs.path} {}).${attrset}')
      EXPECTED='${/* the verbatim expected Tvix output: */ "=> \"${expectedPath}\" :: string"}'

      echo "Tvix output: ''${TVIX_OUTPUT}"
      if [ "$TVIX_OUTPUT" != "$EXPECTED" ]; then
        echo "Correct would have been ''${EXPECTED}"
        exit 1
      fi

      echo "Output was correct."
    '';
  };

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

in

(depot.tvix.crates.workspaceMembers.tvix-cli.build.override {
  runTests = true;
}).overrideAttrs (finalAttrs: previousAttrs:

let
  tvix-cli = finalAttrs.finalPackage;

  # You can run the benchmark with a simple `nix run`, like:
  #
  #  nix run -f . tvix.cli.meta.ci.extraSteps.benchmark-nixpkgs-cross-hello-outpath
  #
  # TODO(amjoseph): store these results someplace more durable, like git trailers
  #
  mkExprBenchmark = { expr, description }:
    let name = "tvix-cli-benchmark-${description}"; in
    (pkgs.writeShellScriptBin name ''
      ${lib.escapeShellArgs [
        "${pkgs.time}/bin/time"
        "--format" "${benchmark-gnutime-format-string description}"
        "${tvix-cli}/bin/tvix"
        "--no-warnings"
        "-E" expr
      ]}
    '').overrideAttrs (finalAttrs: previousAttrs: {
      passthru = (previousAttrs.passthru or { }) // {
        ci = {
          label = ":nix: benchmark ${description} in tvix";
          needsOutput = true;
          command = "${finalAttrs.finalPackage}/bin/${finalAttrs.meta.mainProgram}";
        };
      };
    });

  mkNixpkgsBenchmark = attrpath:
    mkExprBenchmark {
      description = builtins.replaceStrings [ ".drv" ] [ "-drv" ] attrpath;
      expr = "(import ${pkgs.path} {}).${attrpath}";
    };

  benchmarks = {
    benchmark-hello = (mkNixpkgsBenchmark "hello.outPath");
    benchmark-cross-hello = (mkNixpkgsBenchmark "pkgsCross.aarch64-multiplatform.hello.outPath");
    benchmark-firefox = (mkNixpkgsBenchmark "firefox.outPath");
    benchmark-cross-firefox = (mkNixpkgsBenchmark "pkgsCross.aarch64-multiplatform.firefox.outPath");
    # Example used for benchmarking LightSpan::Delayed in commit bf286a54bc2ac5eeb78c3d5c5ae66e9af24d74d4
    benchmark-nixpkgs-attrnames = (mkExprBenchmark { expr = "builtins.length (builtins.attrNames (import ${pkgs.path} {}))"; description = "nixpkgs-attrnames"; });
  };
in
{
  meta = {
    ci.extraSteps = {
      eval-nixpkgs-stdenv-drvpath = (mkNixpkgsEvalCheck "stdenv.drvPath" pkgs.stdenv.drvPath);
      eval-nixpkgs-stdenv-outpath = (mkNixpkgsEvalCheck "stdenv.outPath" pkgs.stdenv.outPath);
      eval-nixpkgs-hello-outpath = (mkNixpkgsEvalCheck "hello.outPath" pkgs.hello.outPath);
      eval-nixpkgs-firefox-outpath = (mkNixpkgsEvalCheck "firefox.outPath" pkgs.firefox.outPath);
      eval-nixpkgs-firefox-drvpath = (mkNixpkgsEvalCheck "firefox.drvPath" pkgs.firefox.drvPath);
      eval-nixpkgs-cross-stdenv-outpath = (mkNixpkgsEvalCheck "pkgsCross.aarch64-multiplatform.stdenv.outPath" pkgs.pkgsCross.aarch64-multiplatform.stdenv.outPath);
      eval-nixpkgs-cross-hello-outpath = (mkNixpkgsEvalCheck "pkgsCross.aarch64-multiplatform.hello.outPath" pkgs.pkgsCross.aarch64-multiplatform.hello.outPath);
    };
    ci.targets = builtins.attrNames benchmarks;
  };

  # Expose benchmarks as standard CI targets.
  passthru = benchmarks;
})
