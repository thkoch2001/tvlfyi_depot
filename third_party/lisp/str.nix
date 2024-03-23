{ depot, pkgs, ... }:

let src = with pkgs; srcOnly lispPackages.str;
in depot.nix.buildLisp.library {
  name = "str";

  deps = with depot.third_party.lisp; [
    { sbcl = depot.nix.buildLisp.bundled "uiop"; }
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
}
