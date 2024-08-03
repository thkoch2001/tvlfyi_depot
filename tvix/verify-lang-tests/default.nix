# SPDX-License-Identifier: LGPL-2.1-only
# SPDX-FileCopyrightText: © 2022 The TVL Contributors
# SPDX-FileCopyrightText: © 2004-2022 The Nix Contributors
#
# Execute language tests found in tvix_tests and nix_tests
# using the C++ Nix implementation. Based on NixOS/nix:tests/lang.sh.
{ depot, pkgs, lib, ... }:

let
  testRoot = ../eval/src/tests;

<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
  # The minimum Nix version we've verified to work for our testing suite.
  nix_minimum_verified = pkgs.nixVersions.nix_2_3;
  # The latest Nix version we've verified to work for our testing suite.
  nix_latest_verified = pkgs.nixVersions.nix_2_23;
=======
  inherit (pkgs.nixVersions) minimum latest;
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)

  parseTest = dir: baseName:
    let
      tokens = builtins.match "(eval|parse)-(okay|fail).+\\.nix" baseName;
    in
    if tokens == null
    then null
    else {
      type = builtins.elemAt tokens 0;
      expectedSuccess = (builtins.elemAt tokens 1) == "okay";
      fileName = "${dir}/${baseName}";
    };

  allLangTests =
    lib.concatMap
      (
        dir:
        lib.pipe
          (builtins.readDir (testRoot + "/${dir}"))
          [
            builtins.attrNames
            (builtins.map (parseTest dir))
            (builtins.filter (t: t != null))
          ]
      ) [ "nix_tests" "nix_tests/notyetpassing" "tvix_tests" "tvix_tests/notyetpassing" ];

  skippedLangTests = {
    # TODO(sterni): set up NIX_PATH in sandbox
    "eval-okay-search-path.nix" = true;
    # Floating point precision differs between tvix and Nix
    "eval-okay-fromjson.nix" = true;
    # C++ Nix can't TCO
    "eval-okay-tail-call-1.nix" = true;
    # Ordering change after 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-xml.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-xml.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # Missing builtins in Nix 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-ceil.nix" = [ nix_minimum_verified ];
    "eval-okay-floor-ceil.nix" = [ nix_minimum_verified ];
    "eval-okay-floor.nix" = [ nix_minimum_verified ];
    "eval-okay-groupBy.nix" = [ nix_minimum_verified ];
    "eval-okay-zipAttrsWith.nix" = [ nix_minimum_verified ];
    "eval-okay-builtins-group-by-propagate-catchable.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-ceil.nix" = [ minimum ];
    "eval-okay-floor-ceil.nix" = [ minimum ];
    "eval-okay-floor.nix" = [ minimum ];
    "eval-okay-groupBy.nix" = [ minimum ];
    "eval-okay-zipAttrsWith.nix" = [ minimum ];
    "eval-okay-builtins-group-by-propagate-catchable.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # Comparable lists are not in Nix 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-sort.nix" = [ nix_minimum_verified ];
    "eval-okay-compare-lists.nix" = [ nix_minimum_verified ];
    "eval-okay-value-pointer-compare.nix" = [ nix_minimum_verified ];
    "eval-okay-builtins-genericClosure-pointer-equality.nix" = [ nix_minimum_verified ];
    "eval-okay-list-comparison.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-sort.nix" = [ minimum ];
    "eval-okay-compare-lists.nix" = [ minimum ];
    "eval-okay-value-pointer-compare.nix" = [ minimum ];
    "eval-okay-builtins-genericClosure-pointer-equality.nix" = [ minimum ];
    "eval-okay-list-comparison.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # getAttrPos gains support for functionArgs-returned sets after 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-getattrpos-functionargs.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-getattrpos-functionargs.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # groupBy appeared (long) after 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-builtins-groupby-thunk.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-builtins-groupby-thunk.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # import is no longer considered a curried primop in Nix > 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-import-display.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-import-display.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # Cycle detection and formatting changed sometime after Nix 2.3
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-cycle-display-cpp-nix-2.13.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-cycle-display-cpp-nix-2.13.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # builtins.replaceStrings becomes lazier in Nix 2.16
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-replacestrings.nix" = [ nix_minimum_verified ];
    # builtins.readFileType is added in Nix 2.15
    "eval-okay-readFileType.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-replacestrings.nix" = [ minimum ];
    # builtins.readFileType is added in Nix 2.14
    "eval-okay-readFileType.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # builtins.fromTOML gains support for timestamps in Nix 2.16
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-fromTOML-timestamps.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-fromTOML-timestamps.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    # identifier formatting changed in Nix 2.17 due to cppnix commit
    # b72bc4a972fe568744d98b89d63adcd504cb586c
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-identifier-formatting.nix" = [ nix_minimum_verified ];
=======
    "eval-okay-identifier-formatting.nix" = [ minimum ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)

    # Differing strictness in the function argument for some builtins in Nix 2.18
    # https://github.com/NixOS/nix/issues/9779
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-builtins-map-propagate-catchable.nix" = [ nix_latest_verified ];
    "eval-okay-builtins-gen-list-propagate-catchable.nix" = [ nix_latest_verified ];
=======
    "eval-okay-builtins-map-propagate-catchable.nix" = [ latest ];
    "eval-okay-builtins-gen-list-propagate-catchable.nix" = [ latest ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
    "eval-okay-builtins-replace-strings-propagate-catchable.nix" =
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
      [ nix_latest_verified ];
    "eval-okay-builtins-map-function-strictness.nix" = [ nix_latest_verified ];
    "eval-okay-builtins-genList-function-strictness.nix" = [ nix_latest_verified ];
=======
      [ latest ];
    "eval-okay-builtins-map-function-strictness.nix" = [ latest ];
    "eval-okay-builtins-genList-function-strictness.nix" = [ latest ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)

    # TODO(sterni): support diffing working directory and home relative paths
    # like C++ Nix test suite (using string replacement).
    "eval-okay-path-antiquotation.nix" = true;

    # The output of dirOf (and maybe other filesystem builtins) have changed in Nix 2.23
    # when they switched to using std::filesystem, https://github.com/NixOS/nix/pull/10658.
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
    "eval-okay-dirof.nix" = [ nix_latest_verified ];
=======
    "eval-okay-dirof.nix" = [ latest ];
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
  };

  runCppNixLangTests = cpp-nix:
    let
      testCommand = { fileName, type, expectedSuccess, ... }:
        let
          testBase = lib.removeSuffix ".nix" fileName;
          expFile =
            let
              possibleFiles =
                builtins.filter
                  (path: builtins.pathExists (testRoot + "/${path}"))
                  (builtins.map
                    (ext: "${testBase}.${ext}")
                    [ "exp" "exp.xml" ]);
            in
            if possibleFiles == [ ] then null else builtins.head possibleFiles;
          outFile = "${testBase}.out";

          # Skip if skippedLangTests prescribes it (possibly just for the current nix)
          # or if we are missing an exp file for an eval-okay test.
          skip =
            let
              doSkip = skippedLangTests.${builtins.baseNameOf fileName} or false;
            in
            if type == "eval" && expectedSuccess && (expFile == null) then true
            else if builtins.isBool doSkip then doSkip
            else builtins.any (drv: cpp-nix == drv) doSkip;

          flagsFile = "${testBase}.flags";

          instantiateFlags =
            lib.escapeShellArgs
              (
                [ "--${type}" fileName ]
                ++ lib.optionals (type == "eval") [ "--strict" ]
                ++ lib.optionals (expFile != null && lib.hasSuffix "xml" expFile)
                  [
                    "--no-location"
                    "--xml"
                  ]
              )
            + lib.optionalString (builtins.pathExists (testRoot + "/${flagsFile}"))
              " $(cat '${flagsFile}')";
        in

        if skip
        then "echo \"SKIP ${type} ${fileName}\"\n"
        else ''
          thisTestPassed=true

          echo "RUN  ${type} ${fileName} ${
            lib.optionalString (!expectedSuccess) "(expecting failure)"
          }"

          if ! expect ${if expectedSuccess then "0" else "1"} \
                 nix-instantiate ${instantiateFlags} \
                 ${if expectedSuccess then "1" else "2"}> \
                 ${if expFile != null then outFile else "/dev/null"};
          then
            echo -n "FAIL"
            thisTestPassed=false
          fi
        '' + lib.optionalString (expFile != null) ''
          if ! diff --color=always -u '${outFile}' '${expFile}'; then
            thisTestPassed=false
          fi
        '' + ''
          if $thisTestPassed; then
            echo -n "PASS"
          else
            echo -n "FAIL"
            passed=false
          fi

          echo " ${type} ${fileName}"

          unset thisTestPassed
        '';
    in

    pkgs.stdenv.mkDerivation {
      name = "cpp-${cpp-nix.name}-run-lang-tests";

      nativeBuildInputs = [ cpp-nix ];

      # Obtain tests via the unpackPhase
      src = testRoot;
      dontConfigure = true;

      # Environment expected by the test suite
      TEST_VAR = "foo";

      buildPhase = ''
        # Make nix-instantiate happy in the sandbox
        export NIX_STORE_DIR="$(realpath "$(mktemp -d store.XXXXXXXXXX)")"
        export NIX_STATE_DIR="$(realpath "$(mktemp -d state.XXXXXXXXXX)")"

        # Helper function to check expected exit code
        expect() {
          local expected res
          expected="$1"
          shift
          set +e
          "$@"
          res="$?"
          set -e
          [[ $res -eq $expected ]]
        }

        # Track test results so far
        passed=true

        source "$testCommandsPath"
      '';

      # Actually runs into the argv limit
      passAsFile = [ "testCommands" ];
      testCommands = lib.concatMapStrings testCommand allLangTests;

      installPhase = ''
        if $passed; then
          touch $out
        else
          echo "Some test(s) failed!"
          exit 1
        fi
      '';
    };

in
depot.nix.readTree.drvTargets {
<<<<<<< PATCH SET (b1d93a feat(tvix/verify-lang-tests): Test nixVersions.nix_{2_3,2_23)
  "nix-${lib.versions.majorMinor nix_minimum_verified.version}" = runCppNixLangTests nix_minimum_verified;
  "nix-${lib.versions.majorMinor nix_latest_verified.version}" = lib.warnIf (lib.versionOlder nix_latest_verified.version pkgs.nixVersions.latest.version)
    "The latest verified Nix version is out of date, consider updating the value of `nix_latest_verified` and verifying that the tests still pass."
    runCppNixLangTests
    nix_latest_verified;
=======
  "nix-${lib.versions.majorMinor minimum.version}" = runCppNixLangTests minimum;
  "nix-${lib.versions.majorMinor latest.version}" = runCppNixLangTests latest;
>>>>>>> BASE      (cd2158 feat(tvix/eval): Re-enable testing for eval-okay-readDir)
}
