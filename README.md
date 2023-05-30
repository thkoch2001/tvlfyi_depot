# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

*This is only a proof-of-concept and is not useful in any meaningful way.*

## Example configuration
```
? <nixspace ?nixspace> $nixspace [

  ? <realise "/nix/store/sv1yikjpf7q8b9w4xszb2ipg0cgcq1xv-imv-4.4.0.drv" ?outputs> [ ]

  ? <eval "3 * 4" {} _> []
  ? <eval "builtins.getEnv \"PATH\"" {impure: ""} _> []

  ? ?any [
    $log ! <log "-" { nix: $any }>
  ]

  $config [
    <require-service <daemon nix_actor>>
    ? <service-object <daemon nix_actor> ?cap> [
      $cap {
        dataspace: $nixspace
      }
    ]
    <daemon nix_actor {
      argv: "/usr/local/nix_actor"
      protocol: application/syndicate
    }>
  ]
]
```
