{ depot, pkgs, ... }:

let
  mblog = depot.nix.buildLisp.program {
    name = "mblog";

    srcs = [
      ./packages.lisp
      ./transformer.lisp
      ./mblog.lisp
    ];

    deps = [
      (depot.nix.buildLisp.bundled "asdf") # for UIOP
      depot.third_party.lisp.alexandria
      depot.third_party.lisp.closure-html
      depot.third_party.lisp.cl-who
      depot.third_party.lisp.mime4cl
    ];

    main = "mblog:main";
  };
in

pkgs.runCommandNoCC "mblog" {
  inherit mblog;
  nativeBuildInputs = [ pkgs.buildPackages.makeWrapper ];
  passthru = {
    unwrapped = mblog;
  };
} ''
  mkdir -p "$out/bin"
  for bin in mblog mnote-html; do
    ln -s "$mblog/bin/mblog" "$out/bin/$bin"
  done
''
