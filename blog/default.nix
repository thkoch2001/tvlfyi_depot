{
  nixpkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  briefcase ? import <briefcase> {},
  ...
}:

let
  injectedPosts = nixpkgs.writeText "posts.lisp" ''
    (in-package #:server)
    (setq *path-to-posts* "${./posts}")
  '';
  injectedExecutables = nixpkgs.writeText "executables.lisp" ''
    (in-package #:server)
    (setq *pandoc-bin* "${nixpkgs.pandoc}/bin/pandoc")
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
    injectedPosts
    injectedExecutables
  ];
}
