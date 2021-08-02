# Copyright (C) 2021 by the TVL Authors
# SPDX-License-Identifier: LGPL-2.1-or-later
{ depot, pkgs, ... }:

depot.nix.buildLisp.library {
  name = "sclf";

  deps = [
    (depot.nix.buildLisp.bundled "sb-posix")
  ];

  srcs = [
    ./package.lisp
    ./sclf.lisp
    ./sysproc.lisp
    ./lazy.lisp
    ./time.lisp
    ./directory.lisp
    ./serial.lisp
    ./mp/sbcl.lisp
  ];

  # TODO(sterni): implement OS interaction for ECL and CCL
  brokenOn = [
    "ecl"
    "ccl"
  ];
}
