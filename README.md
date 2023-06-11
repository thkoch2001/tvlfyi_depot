# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

See [protocol.prs](./protocol.prs) for the Syndicate protocol [schema](https://preserves.dev/preserves-schema.html).

*This is only a proof-of-concept and is not yet useful.*

## Example configuration

A demo script for the [Syndicate server](https://git.syndicate-lang.org/syndicate-lang/syndicate-rs), see https://synit.org/book/operation/scripting.html
```
? <nixspace ?nixspace> $nixspace [

  ? <instantiate "let pkgs = import <nixpkgs> {}; in pkgs.hello" { } ?drv> [
    ? <realise $drv ?outputs> [
      $log ! <log "-" { "hello": $outputs }>
    ]
  ]

  ? <eval "3 * 4" {} ?result> [
    $log ! <log "-" { "nix eval 3 * 4": $result }>
  ]

  ? <eval "builtins.getEnv \"PATH\"" {impure: ""} ?result> [
    $log ! <log "-" { "nix impure path": $result }>
  ]

  ? <missing ["/nix/store/p7fnjrbvmpwl192ir8p2ixfym68j7sgv-invidious-unstable-2023-05-08"] _ ?subs _ ?dlSize ?narSize> [
    $log ! <log "-" { invidious-unstable-2023-05-08: {
      substitutes: $subs
      downloadSize: $dlSize
      narSize: $narSize
    } }>
  ]

  ? <path-info "/nix/store/jhgh02lyizd1kyl71brvc01ygsmgi40a-tzdata-2023c" ?deriver ?narHash _ _ ?narSize _ ?sigs _> [
    $log ! <log "-" { tzdata-2023c: {
      deriver: $deriver
      narHash: $narHash
      narSize: $narSize
      sigs: $sigs
    } }>

  ]

  $config [
    <require-service <daemon nix_actor>>
    ? <service-object <daemon nix_actor> ?cap> [
      $cap {
        dataspace: $nixspace
        daemon-socket: "/nix/var/nix/daemon-socket/socket"
        listen-socket: "/tmp/translator.worker.nix.socket"
      }
    ]
    <daemon nix_actor {
      argv: "/bin/nix_actor"
      protocol: application/syndicate
    }>
  ]
]
```
