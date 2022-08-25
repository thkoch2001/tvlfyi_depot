{ depot, pkgs, lib, ... }:

depot.third_party.naersk.buildPackage {
  src = ./.;
  doCheck = true;

  meta.ci.extraSteps.cpp-nix-run-lang-tests = {
    label = "Run tvix language tests using C++ Nix implementation";
    script = pkgs.writers.writeBash "nix-verify-lang-tests" ''
      # SPDX-License-Identifier: LGPL-2.1-only
      # SPDX-FileCopyrightText: © 2022 The TVL Contributors
      # SPDX-FileCopyrightText: © 2004-2022 The Nix Contributors
      #
      # Execute language tests found in tvix_tests and nix_tests
      # using the C++ Nix implementation. Based on NixOS/nix:tests/lang.sh.

      set +x

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

      unset NIX_PATH

      TESTDIR="$(git rev-parse --show-toplevel)/tvix/eval/src/tests"
      readonly TESTDIR

      export PATH="${lib.makeBinPath [ pkgs.coreutils pkgs.diffutils pkgs.nix ]}"

      fail=0

      for i in "$TESTDIR/"*_tests/parse-fail-*.nix; do
          echo "parsing $i (should fail)";
          if ! expect 1 nix-instantiate --parse - < $i 2> /dev/null; then
              echo "FAIL: $i shouldn't parse"
              fail=1
          fi
      done

      for i in "$TESTDIR/"*_tests/parse-okay-*.nix; do
          echo "parsing $i (should succeed)";
          if ! expect 0 nix-instantiate --parse - < $i > /dev/null; then
              echo "FAIL: $i should parse"
              fail=1
          fi
      done

      for i in "$TESTDIR/"*_tests/eval-fail-*.nix; do
          echo "evaluating $i (should fail)";
          if ! expect 1 nix-instantiate --eval $i 2> /dev/null; then
              echo "FAIL: $i shouldn't evaluate"
              fail=1
          fi
      done

      export TEST_VAR="foo"

      for i in "$TESTDIR/"*_tests/eval-okay-*.nix; do
          echo "evaluating $i (should succeed)";

          base="$(dirname "$i")/$(basename "$i" ".nix")"

          if [[ "$(basename "$i")" == "eval-okay-search-path.nix" ]]; then
            # TODO(sterni): fix this test
            echo "SKIPPED: $i"
            continue
          fi

          if test -e $base.exp; then
              flags=
              if test -e $base.flags; then
                  flags=$(cat $base.flags)
              fi
              if ! expect 0 nix-instantiate $flags --eval --strict $base.nix > $base.out; then
                  echo "FAIL: $i should evaluate"
                  fail=1
              elif ! diff $base.out $base.exp; then
                  echo "FAIL: evaluation result of $i not as expected"
                  fail=1
              fi
          fi

          if test -e $base.exp.xml; then
              if ! expect 0 nix-instantiate --eval --xml --no-location --strict \
                      $base.nix > $base.out.xml; then
                  echo "FAIL: $i should evaluate"
                  fail=1
              elif ! cmp -s $base.out.xml $base.exp.xml; then
                  echo "FAIL: XML evaluation result of $i not as expected"
                  fail=1
              fi
          fi
      done

      exit $fail
    '';
  };
}
