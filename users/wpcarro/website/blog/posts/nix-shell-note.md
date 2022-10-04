## Background

I rarely use `nix-shell` for its originally intended purpose of "reproducing the
environment of a derivation for development". Instead, I often use it to put
some executable on my `PATH` for some ad hoc task.

What's `nix-shell`'s "intended purpose"? Let's ask The Man (`man nix-shell`):

> The command nix-shell will build the dependencies of the specified derivation,
> but not the derivation itself. It will then start an interactive shell in
> which all environment variables defined by the derivation path have been set
> to their corresponding values, and the script $stdenv/setup has been
> sourced. This is useful for reproducing the environment of a derivation for
> development.

Because I'm abusing `nix-shell` in this way, I'm liable to forget that
`nix-shell` puts `buildInputs` on `PATH` and *not* the derivation itself. But I
often only want the derivation!

## Solution

Pass the Nix expression to `nix-shell -p`:

```shell
λ nix-shell -p '(import /depot {}).tvix.eval'
```

## Explanation

This works because Nix forwards the arguments passed to `-p` (i.e. `--packages`)
and interpolates them into this expression here: [source][nix-src]

```nix
{ ... }@args:

with import <nixpkgs> args;

(pkgs.runCommandCC or pkgs.runCommand) "shell" {
  buildInputs = [
    # --packages go here
  ];
}
```

So really you can pass-in *any* valid Nix expression that produces a derivation
and `nix-shell` will put its outputs on your `PATH`.

Enjoy!

[nix-src]: https://sourcegraph.com/github.com/NixOS/nix@3ae9467d57188f9db41f85b0e5c41c0c9d141955/-/blob/src/nix-build/nix-build.cc?L266
