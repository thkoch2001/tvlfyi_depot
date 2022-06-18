# JSON encoder & decoder
{ depot, pkgs, ... }:

let
  inherit (depot.nix) buildLisp;

  # https://github.com/sharplispers/cl-json/pull/12/
  src = pkgs.fetchFromGitHub {
    owner = "sternenseemann";
    repo = "cl-json";
    rev = "479685029c511cb2011f2f2a99ca6c63aa2e4865";
    sha256 = "1663xlzb0wj6kd0wy2cmhafrwip7vy0wlfckc519aj9j18aak5ja";
  };

  getSrcs = subdir: map (f: src + ("/" + subdir + "/" + f));
in
buildLisp.library {
  name = "cl-json";
  deps = [ (buildLisp.bundled "asdf") ];

  srcs = [ "${src}/cl-json.asd" ] ++
    (getSrcs "src" [
      "package.lisp"
      "common.lisp"
      "objects.lisp"
      "camel-case.lisp"
      "decoder.lisp"
      "encoder.lisp"
      "utils.lisp"
      "json-rpc.lisp"
    ]);

  tests = {
    deps = [
      depot.third_party.lisp.cl-unicode
      depot.third_party.lisp.fiveam
    ];
    srcs = [
      # CLOS tests are broken upstream as well
      # https://github.com/sharplispers/cl-json/issues/11
      (pkgs.writeText "no-clos-tests.lisp" ''
        (replace *features* (delete :cl-json-clos *features*))
      '')
    ] ++ getSrcs "t" [
      "package.lisp"
      "testencoder.lisp"
      "testdecoder.lisp"
      "testmisc.lisp"
    ];

    expression = "(fiveam:run! 'json-test::json)";
  };
}
