# Solutions for Advent of Code 2020, written in Emacs Lisp.
#
# For each day a new file is created as "solution-day$n.el".
{ depot, pkgs, ... }:

let
  inherit (builtins) attrNames filter head listToAttrs match readDir;
  dir = readDir ./.;
  matchSolution = match "solution-(.*)\.el";
  isSolution = f: (matchSolution f) != null;
  getDay = f: head (matchSolution f);

  solutionFiles = filter (e: dir."${e}" == "regular" && isSolution e) (attrNames dir);
  solutions = map
    (f:
      let day = getDay f; in
      depot.nix.writeElispBin {
        name = day;
        deps = p: with p; [ dash s ht p.f ];
        src = ./. + ("/" + f);
      })
    solutionFiles;
in
pkgs.symlinkJoin {
  name = "aoc2020";
  paths = solutions;
}
