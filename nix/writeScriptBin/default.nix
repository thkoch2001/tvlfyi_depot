{ depot, ... }:

# Like writeScript,
# but put the script into `$out/bin/${name}`.

name: script:

depot.nix.binify {
  exe = (depot.nix.writeScript name script);
  inherit name;
}
