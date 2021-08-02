# Copyright (C) 2021 by the TVL Authors
# SPDX-License-Identifier: LGPL-2.1-or-later
{ depot, pkgs, ... }:

depot.nix.buildLisp.library {
  name = "npg";

  srcs = [
    ./src/package.lisp
    ./src/common.lisp
    ./src/define.lisp
    ./src/parser.lisp
  ];
}
