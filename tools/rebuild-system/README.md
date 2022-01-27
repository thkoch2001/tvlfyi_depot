# rebuild-system

Rebuild your machine's current NixOS. This is a useful tool for upgrading your
machine or deploying software within depot.

## Usage

By default, all builds use the local depot git repository. If you'd like to use
the latest state from `origin/canon`, pass the `--canon` flag. This is akin to
"upgrading":

```shell
rebuild-system --canon
```

For more details, see `--help`:

```shell
rebuild-system --help
```
