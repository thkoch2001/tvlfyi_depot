{ depot, pkgs, ... }:

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

  passthru.meta = {
    extraSteps.verify = {
      label = "verify argv[0] output";
      needsOutput = true;
      command = pkgs.writeShellScript "check-argv0" ''
        set -eux

        for invocation in "$(pwd)/result/bin/argv0-test" "./result/bin/argv0-test"; do
          test "$invocation" = "$("$invocation")"
        done
      '';
    };
  };
}
