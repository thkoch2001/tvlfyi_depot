# whatcd-resolver

To run:

```
ninja run-services
```

in one terminal (starts the background tasks)

```
ninja run
```

to start the server. It runs on `9092`.

You need to be in the `nix-shell` in `./..`.

You need to set the `pass` key `internet/redacted/api-keys/whatcd-resolver` to an API key for RED.

You need to have a transmission-rpc-daemon listening on port `9091` (no auth, try ssh port forwarding lol).
