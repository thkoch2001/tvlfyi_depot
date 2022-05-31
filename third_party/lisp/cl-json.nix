# JSON encoder & decoder
{ depot, pkgs, ... }:

let
  inherit (depot.nix) buildLisp;

  src = pkgs.srcOnly pkgs.quicklispPackages.cl-json;
in
buildLisp.library {
  name = "cl-json";
  deps = [ (buildLisp.bundled "asdf") ];

  srcs = [ "${src}/cl-json.asd" ] ++
    (map (f: src + ("/src/" + f)) [
      "package.lisp"
      "common.lisp"
      "objects.lisp"
      "camel-case.lisp"
      "decoder.lisp"
      "encoder.lisp"
      "utils.lisp"
      "json-rpc.lisp"
    ]);
}
