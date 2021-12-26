{ depot, ... }:
depot.ops.secrets.mkSecrets ./. (import ./secrets.nix)
