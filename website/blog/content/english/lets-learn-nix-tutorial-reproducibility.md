---
title: "Lets Learn Nix: Tutorial Reproducibility"
date: 2020-03-17T18:34:58Z
draft: true
---

## Install Nix

Link to nixos page.

## The rest

Start with this...

```shell
$ mkdir ~/lets-learn-nix
$ cd ~/lets-learn-nix
```

...done. Copy the following and paste it into a file name `shell.nix`.

```nix
# file: shell.nix
let
  pkgs = import (builtins.fetchGit {
    url = "https://github.com/NixOS/nixpkgs-channels";
    ref = "refs/heads/nixos-19.09";
  }) {}
in pkgs.mkShell {
  buildInputs = with pkgs; [
    git
  ];
  NIX_PATH = "nixpkgs=${pkgs}";
};
```

...then...

```shell
$ nix-shell
```
