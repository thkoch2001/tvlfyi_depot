# briefcase

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
 CSS                     9        67324        50733          218        16373
 Emacs Lisp            111        25326        15790         6337         3199
 Python                 99         7432         5414          623         1395
 JSON                   18         2235         2235            0            0
 Markdown               34         1771         1771            0            0
 TypeScript             25         1665         1317          115          233
 Nix                    65         1302         1115           82          105
 Go                     17         1256          926          173          157
 Vim Script              2          766          470           87          209
 HTML                   17          496          459           11           26
 Org                     8          420          411            8            1
 Haskell                 4          319          217           57           45
 Plain Text              5          145          145            0            0
 JavaScript             13          105           99            0            6
 Fish                    1           87           54           23           10
 Lisp                    3           83           43           23           17
 Elixir                  1           50           39            5            6
 Sass                    1           51           38            2           11
 TOML                    2           37           32            0            5
 Shell                   2           34           15            9           10
 Java                    2           11           11            0            0
 Makefile                2           14            9            3            2
 C                       1            6            5            0            1
 BASH                    2           10            4            2            4
 YAML                    1            5            4            0            1
 Rust                    1            5            3            1            1
-------------------------------------------------------------------------------
 Total                 446       110955        81359         7779        21817
-------------------------------------------------------------------------------
```

67,321 of the 67,324 lines of CSS comes from `//website`, which includes the
template I use for my blog. Because I use TailwindCSS for my personal projects,
most of the styling is embedded in the class atribute of HTML and JSX tags.

## Sign posts

Below I have outlined a few projects that you might find interesting. I am
using `//` to indicate the root of my monorepo, the directory in which this
`README.md` resides.

- `//boilerplate`: scaffolding for projects. Boilerplate's goal is to
  reduce the startup costs of a project.
- `//configs`: my dotfiles (e.g. `config.fish`, `init.vim`). Eventually Nix
  `home-manager` should replace this.
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

- Use Nix `home-manager` to configure the new machine.
- Ensure `~/.password-store` exists.
- Run `export_gpg` from a computer with my gpg credentials. Run `import_gpg`
  from the new machine.
- Ensure the new machine can access my Github.
