## Background

Much in the same vain as my [nix-shell (note to self)][nix-shell-note], I'm
going to leave a note to my future self on how to install packages using
`nix-env`, which is something I do once in a blue moon.

## Solution

```shell
λ nix-env -iA tvix.eval -f /depot
```

Looks like I was forgetting the `-f /depot` option all this time:

> --file / -f path
>     Specifies the Nix expression (designated below as the active Nix
>     expression) used by the --install, --upgrade, and --query --available
>     operations to obtain derivations. The default is ~/.nix-defexpr.
> - `man nix-env`

## Failed Attempts (don't try these at home)

This section is brought to you by my shell's `Ctrl-r`!

```shell
λ nix-env -I depot=/depot -iA depot.tvix.eval
λ NIX_PATH=depot=/depot nix-env -iA depot.tvix.eval
λ nix-env -iE '(import /depot {}).tvix.eval'
```

Thanks for reading!

[nix-shell-note]: https://billandhiscomputer.com/blog/posts/nix-shell.html
