# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

See [protocol.prs](./protocol.prs) for the Syndicate protocol [schema](https://preserves.dev/preserves-schema.html).

The was once an abstraction of the Nix worker socket that could intermediate between clients and the worker but that code has been removed, refer to git history for that.

*This is only a proof-of-concept and is probably not useful yet.*

## Example configuration

A demo script for the [Syndicate server](https://git.syndicate-lang.org/syndicate-lang/syndicate-rs), see https://synit.org/book/operation/scripting.html
```
#!/usr/bin/env -S syndicate-server --config

let ?nixLog = dataspace
$nixLog ?? ?line [
  # Log out to the syndicate-server log.
  $log ! <log "-" { line: $line }>
]

let ?results = dataspace
$results ? ?any [
  # Just log everything else.
  $log ! <log "-" { line: $any }>
]

let ?resolver = dataspace
$resolver ? <accepted ?cap> $cap [

  <eval "builtins.trace \"This is only a test.\" builtins.nixVersion" $nixLog $results>

  <instantiate "let pkgs = import <nixpkgs> {}; in pkgs.cowsay" $nixLog $results>

  $results ? <drv ?expr ?drv> [
    $log ! <log "-" { line: "realise" expr: $expr }>
    $cap <realise $drv $nixLog $results>
  ]

]

? <service-object <daemon nix-actor> ?cap> [
  $cap <resolve <nix-actor {
      command-path: [ "/run/current-system/sw/bin" ]
      lookupPath: [
        "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixos",
        "nixos-config=/etc/nixos/configuration.nix",
        "/nix/var/nix/profiles/per-user/root/channels",
      ]
      options: { }
    }> $resolver>
]

<require-service <daemon nix-actor>>

<daemon nix-actor {
  argv: [ $nix-actor-path ]
  clearEnv: #t
  env: {
    BUILD_SUM: $sum
  }
  protocol: application/syndicate
}>
```
