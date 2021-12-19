{ depot, lib, pkgs, ... }:

let
  deps = with depot.third_party.lisp; [
    bordeaux-threads
    cl-json
    cl-ppcre
    cl-smtp
    cl-who
    defclass-std
    drakma
    easy-routes
    hunchentoot
    lass
    local-time
    postmodern
    trivial-ldap

    depot.lisp.klatre

      fiveam
  ];

  srcs = [
    ./panettone.asd
    ./src/packages.lisp
    ./src/util.lisp
    ./src/css.lisp
    ./src/email.lisp
    ./src/inline-markdown.lisp
    ./src/authentication.lisp
    ./src/model.lisp
    ./src/irc.lisp
    ./src/panettone.lisp
  ];
in depot.nix.buildLisp.program {
  name = "panettone";

  inherit deps srcs;

  tests = {
    deps = with depot.third_party.lisp; [
      fiveam
    ];

    srcs = [
      ./test/package.lisp
      ./test/model_test.lisp
      ./test/inline-markdown_test.lisp
    ];

    expression = "(fiveam:run!)";
  };

  brokenOn = [
    "ecl" # dependencies use dynamic cffi
    "ccl" # The value NIL is not of the expected type STRING. when loading model.lisp
  ];

  passthru = rec {
    integrationTests = depot.nix.buildLisp.runTestSuite {
      implementation = depot.nix.buildLisp.sbcl;
      name = "panettone-integration-tests";

      srcs = srcs ++ [
        ./test/package.lisp
        ./test/integration_test.lisp
      ];

      deps = deps ++ (with depot.third_party.lisp; [
        fiveam
      ]);

      expression = "(fiveam:run!)";
    };

    # Only build one copy
    meta.targets = [];

    meta.extraSteps = [{
      label = "Panettone Integration Tests";
      command = let
        inherit (pkgs)
          arion
          writeShellScript
        ;

      in writeShellScript "panettone-tests.sh" ''
        set -euo pipefail

        export PATH="$PATH:${pkgs.lib.makeBinPath (with pkgs; [
          docker
          arion
        ])}"

        cd web/panettone
        trap "arion down" EXIT
        arion run panettone-integration-tests
      '';
    }];
  };
}
