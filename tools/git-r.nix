# Git subcommand loaded into the depot direnv via //tools/depot-deps that can
# display the r/number for (a) given commit(s) in depot. The r/number is a
# monotonically increasing number assigned to each commit which correspond to
# refs/r/* as created by `//ops/pipelines/static-pipeline.yaml`. They can also
# be used as TVL shortlinks and are supported by //web/atward.
{ pkgs, lib, ... }:

pkgs.writeTextFile {
  name = "git-r";
  destination = "/bin/git-r";
  executable = true;
  text = ''
      set -euo pipefail

      PROG_NAME="$0"

      CANON_BRANCH="canon"
      CANON_REMOTE="$(git config "branch.$CANON_BRANCH.remote")"
      CANON_HEAD="$CANON_REMOTE/$CANON_BRANCH"

      usage() {
        cat <<EOF
    Usage: git r [-h | --usage] [<git commit> ...]

      Display the r/number for the given git commit(s). If none is given,
      HEAD is used as a default. The r/number is a monotonically increasing
      number assigned to each commit on the $CANON_BRANCH branch in depot
      equivalent  to the revcount ignoring merged in branches (using
      git-rev-list(1) internally).

      The r/numbers displayed by \`git r\` correspond to refs created by CI
      in depot, so they can be used as monotonically increasing commit
      identifiers that can be used instead of a commit hash. To have
      \`refs/r/*\` available locally, you may have to enable fetching them
      like this:

          git config --add remote.origin.fetch '+refs/r/*:refs/r/*'

    EOF
        exit "''${1:-0}"
      }

      eprintf() {
        printf "$@" 1>&2
      }

      revs=()

      if [[ $# -le 0 ]]; then
        revs+=("HEAD")
      fi

      for arg in "$@"; do
        # No flags supported at the moment
        case "$arg" in
          # --help is mapped to `man git-r` by git(1)
          # TODO(sterni): git-r man page
          -h | --usage)
            usage
            ;;
          -*)
            eprintf 'error: unknown flag %s\n' "$PROG_NAME" "$arg"
            usage 100 1>&2
            ;;
          *)
            revs+=("$arg")
            ;;
        esac
      done

      for rev in "''${revs[@]}"; do
        # Make sure $rev is well formed
        git rev-parse "$rev" -- > /dev/null

        if git merge-base --is-ancestor "$rev" "$CANON_HEAD"; then
          printf 'r/'
          git rev-list --count --first-parent "$rev"
        else
          eprintf 'error: refusing to calculate r/number: %s is not an ancestor of %s\n' \
            "$rev" "$CANON_HEAD" 1>&2
          exit 100
        fi
      done
  '';

  # Test case, assumes that it is executed in a checkout of depot
  meta.ci.extraSteps.matches-refs = {
    needsOutput = true;
    label = "Verify `git r` output matches refs/r/*";
    command = pkgs.writeShellScript "git-r-matches-refs" ''
      set -euo pipefail

      export PATH="${lib.makeBinPath [ pkgs.git pkgs.findutils ]}"
      revs=("origin/canon" "origin/canon~1" "93a746aaaa092ffc3e7eb37e1df30bfd3a28435f")

      failed=false

      # assert_eq DESCRIPTION EXPECTED GIVEN
      assert_eq() {
        desc="$1"
        exp="$2"
        given="$3"

        if [[ "$exp" != "$given" ]]; then
          failed=true
          printf 'error: case "%s" failed\n\texp:\t%s\n\tgot:\t%s\n' "$desc" "$exp" "$given" 1>&2
        fi
      }

      git fetch origin '+refs/r/*:refs/r/*'

      for rev in "''${revs[@]}"; do
        assert_eq \
          "r/number ref for $rev points at that rev" \
          "$(git rev-parse "$rev")" \
          "$(git rev-parse "$(./result/bin/git-r "$rev")")"
      done

      for rev in "''${revs[@]}"; do
        assert_eq \
          "r/number for matches ref pointing at $rev" \
          "$(git for-each-ref --points-at="$rev" --format="%(refname:short)" 'refs/r/*')" \
          "$(./result/bin/git-r "$rev")"
      done

      assert_eq \
        "Passing multiple revs to git r works as expected" \
        "$(git rev-parse "''${revs[@]}")" \
        "$(./result/bin/git-r "''${revs[@]}" | xargs git rev-parse)"

      if $failed; then
        exit 1
      fi
    '';
  };
}
