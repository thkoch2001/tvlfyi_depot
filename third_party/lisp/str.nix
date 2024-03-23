{ depot, pkgs, ... }:

let
  inherit (depot.nix) buildLisp;
  src = with pkgs; srcOnly lispPackages.str;
in
buildLisp.library {
  name = "str";

  deps = with depot.third_party.lisp; [
    {
      sbcl = buildLisp.bundled "uiop";
      default = buildLisp.bundled "asdf";
    }
    cl-ppcre
    cl-ppcre.unicode
    cl-change-case
  ];

  srcs = [
    (pkgs.runCommand "str.lisp" { } ''
      substitute ${src}/str.lisp $out \
        --replace-fail \
          '(asdf:component-version (asdf:find-system "str"))' \
          '"${pkgs.lispPackages.str.meta.version}"'
    '')
  ];

  brokenOn = [
    "ccl" # In REPLACE-USING: Shouldn't assign to variable I
  ];
}
