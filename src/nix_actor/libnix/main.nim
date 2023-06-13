# SPDX-FileCopyrightText: ☭ Emery Hemingway
# SPDX-License-Identifier: Unlicense

{.passC: staticExec("pkg-config --cflags nix-main").}
{.passL: staticExec("pkg-config --libs nix-main").}

proc initNix*() {.importcpp: "nix::initNix", header: "shared.hh".}
