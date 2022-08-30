# JSON encoder & decoder
{ depot, pkgs, ... }:

let
  inherit (depot.nix) buildLisp;

  # https://github.com/sharplispers/cl-json/pull/12/
  src = pkgs.fetchFromGitHub {
    owner = "sternenseemann";
    repo = "cl-json";
    rev = "c059bec94e28a11102a994d6949e2e52764f21fd";
    sha256 = "0l07syw1b1x2zi8kj4iph3rf6vi6c16b7fk69iv7x27wrdsr1qwj";
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
