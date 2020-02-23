{ pkgs, depot, briefcase, ... }:

let
  injections = pkgs.writeText "injections.lisp" ''
    (in-package #:server)
    (setq *path-to-posts* "${./posts}")
    (setq *pandoc-bin* "${pkgs.pandoc}/bin/pandoc")
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
