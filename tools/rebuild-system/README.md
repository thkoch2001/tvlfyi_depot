# rebuild-system

Rebuild your machine's current NixOS. This is a useful tool for upgrading your
machine or deploying software within depot.

## Usage

By default, all builds are local. If you'd like to use the latest state from
`origin/canon`, pass the `--remote` flag:

```shell
rebuild-system --remote
```

For more details, see `-help`:

```shell
rebuild-system -help
```
