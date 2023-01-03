# Shell derivation to invoke //nix/lazy-deps with the dependencies that should
# be lazily made available in wpcarro's users dir in depot.
{ pkgs, depot, ... }:

depot.nix.lazy-deps {
  import-gpg.attr = "users.wpcarro.configs.import-gpg";
  export-gpg.attr = "users.wpcarro.configs.export-gpg";
}
