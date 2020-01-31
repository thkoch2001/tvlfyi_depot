{
  nixpkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  briefcase ? import <briefcase> {},
  ...
}:

let
  injections = nixpkgs.writeText "injections.lisp" ''
    (in-package #:server)
    (setq *path-to-posts* "${./posts}")
    (setq *pandoc-bin* "${nixpkgs.pandoc}/bin/pandoc")
    (setq *html-template* "${./src/index.html}")
  '';
in depot.nix.buildLisp.program {
  name = "server";
  deps = with depot.third_party.lisp; with briefcase.third_party.lisp; [
    hunchentoot
    cl-arrows
    cl-ppcre
  ];
  srcs = [
    ./src/server.lisp
    injections
  ];
}
