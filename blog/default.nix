{
  nixpkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  universe ? import <universe> {},
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
  deps = with depot.third_party.lisp; with universe.third_party.lisp; [
    hunchentoot
    cl-arrows
  ];
  srcs = [
    ./src/server.lisp
    injectedPosts
    injectedExecutables
  ];
}
