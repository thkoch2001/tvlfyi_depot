# Elixir Tooling

Disclaimer: This file is intended to document learnings about tooling text editors to work with Elixir.

## Emacs

### Useful plugins

* `Elixir-mode`: font-locking, indentation, etc.
* `Alchemist`: IEx, mix, autocompletion via `company`, etc.
* `Flycheck`: works with linters, `Dogma` and `Credo`

When running an umbrella app, to allow `Alchemist` to properly jump to function definitions run...

```bash
$ ln -sf ../../_build _build
```

...where `../../_build` is the path to the umbrella root directory.
