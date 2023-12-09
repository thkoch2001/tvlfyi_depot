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
    attrpath:
    "Benchmark: " +
    (builtins.toJSON {
      "${attrpath}" = {
        kbytes = "%M";
        system = "%S";
        user = "%U";
      };
    });

  # You can run the benchmark with a simple `nix run`, like:
  #
  #  nix run -f . tvix.cli.meta.ci.extraSteps.benchmark-nixpkgs-cross-hello-outpath
  #
  # TODO(amjoseph): store these results someplace more durable, like git trailers
  #
  mkNixpkgsBenchmark = attrpath: tvix-cli:
    let name = "tvix-cli-benchmark-${builtins.replaceStrings [".drv"] ["-drv"] attrpath}"; in
    (pkgs.writeShellScriptBin name ''
      ${lib.escapeShellArgs [
        "${pkgs.time}/bin/time"
        "--format" "${benchmark-gnutime-format-string attrpath}"
        "${tvix-cli}/bin/tvix"
        "--no-warnings"
        "-E" "(import ${pkgs.path} {}).${attrpath}"
      ]}
    '').overrideAttrs (finalAttrs: previousAttrs: {
      passthru = (previousAttrs.passthru or { }) // {
        ci = {
          label = ":nix: benchmark nixpkgs.${attrpath} in tvix";
          needsOutput = true;
          command = "${finalAttrs.finalPackage}/bin/${finalAttrs.meta.mainProgram}";
        };
      };
    });
in

(depot.tvix.crates.workspaceMembers.tvix-cli.build.override {
  runTests = true;
}).overrideAttrs (finalAttrs: previousAttrs: {
  meta = {
    ci.extraSteps = {
      eval-nixpkgs-stdenv-drvpath = (mkNixpkgsEvalCheck "stdenv.drvPath" pkgs.stdenv.drvPath);
      eval-nixpkgs-stdenv-outpath = (mkNixpkgsEvalCheck "stdenv.outPath" pkgs.stdenv.outPath);
      eval-nixpkgs-hello-outpath = (mkNixpkgsEvalCheck "hello.outPath" pkgs.hello.outPath);
      eval-nixpkgs-cross-stdenv-outpath = (mkNixpkgsEvalCheck "pkgsCross.aarch64-multiplatform.stdenv.outPath" pkgs.pkgsCross.aarch64-multiplatform.stdenv.outPath);
      eval-nixpkgs-cross-hello-outpath = (mkNixpkgsEvalCheck "pkgsCross.aarch64-multiplatform.hello.outPath" pkgs.pkgsCross.aarch64-multiplatform.hello.outPath);
      benchmark-hello = (mkNixpkgsBenchmark "hello.outPath" finalAttrs.finalPackage);
      benchmark-cross-hello = (mkNixpkgsBenchmark "pkgsCross.aarch64-multiplatform.hello.outPath" finalAttrs.finalPackage);
    };
  };
})
