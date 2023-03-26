# Syndicated Nix Actor

An actor for interacting with the [Nix](https://nixos.org/) daemon via the [Syndicated Actor Model](https://syndicate-lang.org/).

*This is only a proof-of-concept and is not useful in any meaningful way.*

## Example configuration
```
; create and publish a dedicated dataspace
let ?nixspace = dataspace
<nixspace $nixspace>

$nixspace [
  ; request a build of nixpkgs#hello
  ? <nix-build "nixpkgs#hello" ?output> [
    $log ! <log "-" { hello: $output }>
  ]
]

; start nix_actor as a daemon
<require-service <daemon nix_actor>>
<daemon nix_actor {
  argv: "/run/current-system/sw/bin/nix_actor"
  protocol: application/syndicate
}>

; hand-off a capablity to the Nix dataspace to the actor
? <service-object <daemon nix_actor> ?actor> [
  $actor <serve $nixspace>
]
```
