{ depot, pkgs, lib, ... }:

let
  # Trivial test program that outputs argv[0] and exits
  prog =
    depot.nix.buildLisp.program {
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

  # Extract the given implementation's drv (by name) and verify argv[0] output
  checkImplementation = impl:
    pkgs.runCommand "check-argv0" { } ''
      set -eux

      checkInvocation() {
        invocation="$1"
        test "$invocation" = "$("$invocation")"
      }

      checkInvocation "${prog.${impl}}/bin/argv0-test"

      cd ${prog.${impl}}
      checkInvocation "./bin/argv0-test"

      cd bin
      checkInvocation ./argv0-test

      set +x

      touch "$out"
    '';

  inherit (prog.meta.ci) targets;
in

# Wire up a subtarget for every (active) implementation
lib.genAttrs targets checkImplementation // {
  meta.ci = { inherit targets; };
}
