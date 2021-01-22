# briefcase

[![Build status](https://badge.buildkite.com/aa0d413bfeedcafd8719f977eadd40e04d0b5334fc7f58e8ee.svg)](https://buildkite.com/wpcarros-infrastructure/post-receive)

Welcome to my monorepo: briefcase.

Herein you will find a variety of libraries, packages, and documents. Some of
this work in finished and other work is incomplete or just a sketch for a
future project.

Where applicable, I try to include `README.md` files in some of the
subdirectories to help orient both myself and any onlookers.

## Languages

To give you a general idea of the source code inside of this monorepo, here is
the latest output from `tokei --hidden --sort code .`:

```text
-------------------------------------------------------------------------------
 Language            Files        Lines         Code     Comments       Blanks
-------------------------------------------------------------------------------
 Emacs Lisp             81        22267        13847         5661         2759
 Python                177        10575         7930          885         1760
 Elm                    34         5345         4277          219          849
 Haskell                50         4263         3111          428          724
 Nix                    66         1581         1379           66          136
 TypeScript             19         1345         1067           90          188
 Go                     17         1256          926          173          157
 Vim Script              2          766          470           87          209
 Elixir                 13          358          301            8           49
 JavaScript              9           77           73            0            4
 Lisp                    3           83           43           23           17
 Shell                   3           55           30           11           14
 Clojure                 2           10            8            0            2
 C                       1            6            5            0            1
 Rust                    1            5            3            1            1
-------------------------------------------------------------------------------
 Total                 478        47992        33470         7652         6870
-------------------------------------------------------------------------------
```

## Sign posts

Below I have outlined a few projects that you might find interesting. I am
using `//` to indicate the root of my monorepo, the directory in which this
`README.md` resides.

- `//boilerplate`: scaffolding for projects. Boilerplate's goal is to
  reduce the startup costs of a project.
- `//configs`: my dotfiles (e.g. `config.fish`, `init.vim`).
- `//emacs`: Emacs is both my preferred text editor and my window manager; with
  tens of thousands of lines of Emacs Lisp, you can safely assume that this
  directory hosts a lot of libraries and packages.
- `//monzo_ynab`: `systemd` timer unit that imports my Monzo (i.e. a U.K.-based
  online bank) transactions into the personal finance tool YNAB (i.e.
  youneedabudget.com).
- `//nixos`: my declarative configuration for my NixOS machines. If you are
  unfamiliar with Nix, I recommend reading about the NixOS project.
- `//tools`: some scripts and projects that simplify my life.
- `//website`: everything required to build my website, wpcarro.dev.

## Notes to self

Here are a few reminders when setting up a new machine:

- Ensure `~/.password-store` exists.
- Run `export_gpg` from a computer with my gpg credentials. Run `import_gpg`
  from the new machine.
- Ensure the new machine can access my Github.
