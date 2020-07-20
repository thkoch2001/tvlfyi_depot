# SPDX-License-Identifier: Apache-2.0
#
# A crude wrapper around //nix/buildGo that supports the Go 2 alpha.
#
# The way the alpha is implemented is via a transpiler from typed to
# untyped Go.
{ depot, pkgs, ... }:

let
  inherit (builtins)
    baseNameOf
    stringLength
    substring;

  inherit (depot.nix.buildGo) gpackage program;

  go2goext = file: substring 0 ((stringLength file) - 1) file;
  go2go = file: pkgs.runCommandNoCC "${go2goext (baseNameOf file)}" {} ''
    cp ${file} .
    ${pkgs.go}/bin/go tool go2go translate *.go2
    mv *.go $out
  '';

in rec {
  program = { name, srcs, deps ? [], x_defs ? {} }: depot.nix.buildGo.program {
    inherit name deps x_defs;
    srcs = map go2go srcs;
  };

  package = { name, srcs, deps ? [], path ? name, sfiles ? [] }: depot.nix.buildGo.package {
    inherit name deps path sfiles;
    srcs = map go2go srcs;
  };
}
