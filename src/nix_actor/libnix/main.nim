# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

{.passC: staticExec"$PKG_CONFIG --cflags nix-main".}
{.passL: staticExec"$PKG_CONFIG --libs nix-main".}

proc initNix*() {.importcpp: "nix::initNix", header: "shared.hh".}
