# wpcarro

Welcome to my monorepo.

Herein you will find a variety of libraries, packages, and documents. Some of
this work in finished and other work is incomplete or just a sketch for a
future project.

Where applicable, I try to include `README.md` files in some of the
subdirectories to help orient both myself and any onlookers.

## Sign posts

Below I have outlined a few projects that you might find interesting.

- `boilerplate`: scaffolding for projects. Boilerplate's goal is to reduce the
  startup costs of a project.
- `configs`: my dotfiles (e.g. `config.fish`, `init.vim`).
- `emacs`: Emacs is both my preferred text editor and my window manager; with
  tens of thousands of lines of Emacs Lisp, you can safely assume that this
  directory hosts a lot of libraries and packages.
- `monzo_ynab`: `systemd` timer unit that imports my Monzo (i.e. a U.K.-based
  online bank) transactions into the personal finance tool YNAB (i.e.
  youneedabudget.com).
- `nixos`: my declarative configuration for my NixOS machines. If you are
  unfamiliar with Nix, I recommend reading about the NixOS project.
- `tools`: some scripts and projects that simplify my life.
- `website`: everything required to build my website, https://wpcarro.dev.

## Installation

### Google Machine

- ensure `/google-briefcase` exists
- read `/google-briefcase/README.md`

### NixOS Machine

```shell
$ nix-shell -p nixos.{git,direnv}
$ git clone https://code.tvl.fyi/depot.git /depot
$ cd /depot
$ eval "$(direnv hook bash)"
$ HOSTNAME=base rebuild-system
$ sudo tailscale up
$ git clone 'user@host:~/.password-store' ~/.password-store
$ scp -r 'user@host:~/.gnupg' ~/.gnupg
```
