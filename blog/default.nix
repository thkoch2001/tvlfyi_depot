{
  pkgs ? import <nixpkgs> {},
  depot ? import <depot> {},
  universe ? import <universe> {},
  ...
}:

let
  injectedPosts = pkgs.writeText "posts.lisp" ''
    (in-package #:server)
    (setq *path-to-posts* "${./posts}")
  '';
  injectedExecutables = pkgs.writeText "executables.lisp" ''
    (in-package #:server)
    (setq *pandoc-bin* "${pkgs.pandoc}/bin/pandoc")
  '';
in depot.nix.buildLisp.program {
  name = "server";
  deps = with depot.third_party.lisp; [
    hunchentoot
  ];
  srcs = [
    ./src/server.lisp
    injectedPosts
    injectedExecutables
  ];
}
