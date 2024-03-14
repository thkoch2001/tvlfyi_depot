{
  depot,
  pkgs,
  lib,
  ...
}:

let
  # Trivial test program that outputs argv[0] and exits
  prog = depot.nix.buildLisp.program {
    name = "argv0-test";

    srcs = [
      (pkgs.writeText "argv0-test.lisp" ''
        (defpackage :argv0-test (:use :common-lisp :uiop) (:export :main))
        (in-package :argv0-test)

        (defun main ()
          (format t "~A~%" (uiop:argv0)))
      '')
    ];

    deps = [
      {
        sbcl = depot.nix.buildLisp.bundled "uiop";
        default = depot.nix.buildLisp.bundled "asdf";
      }
    ];
  };

  # Extract verify argv[0] output for given buildLisp program
  checkImplementation =
    prog:
    pkgs.runCommand "check-argv0" { } ''
      set -eux

      checkInvocation() {
        invocation="$1"
        test "$invocation" = "$("$invocation")"
      }

      checkInvocation "${prog}/bin/argv0-test"

      cd ${prog}
      checkInvocation "./bin/argv0-test"

      cd bin
      checkInvocation ./argv0-test

      set +x

      touch "$out"
    '';

  inherit (prog.meta.ci) targets;
in

(checkImplementation prog).overrideAttrs (_: {
  # Wire up a subtarget all (active) non-default implementations
  passthru = lib.genAttrs targets (name: checkImplementation prog.${name});

  meta.ci = {
    inherit targets;
  };
})
