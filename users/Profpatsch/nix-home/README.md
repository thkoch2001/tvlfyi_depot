# nix-home

My very much simplified version of [home-manager](https://github.com/nix-community/home-manager/).

Only takes care about installing symlinks into `$HOME`, and uses [`GNU stow`](https://www.gnu.org/software/stow/) for doing the actual mutating.

No support for services (yet).
