{ pkgs, depot, ... }:

let
  inherit (depot.nix)
    getBins
    ;

  stableRelease = "21.05";

  channelsUrl = "https://channels.nixos.org";
  archiveUrl = "https://github.com/NixOS/nixpkgs/archive/";

  bins = getBins pkgs.nix [ "nix-prefetch-url" ]
    //   getBins pkgs.curl [ "curl" ]
    ;

in

pkgs.writers.writeDashBin "depot-nixpkgs-update" ''
  set -e

  printSet() {
    setname="$1"
    shift
    channel="$1"
    shift

    commit="$(${bins.curl} -L "${channelsUrl}/$channel/git-revision")"
    date="$(curl -i -L "${channelsUrl}/$channel/git-revision" \
      | grep ^last-modified \
      | sed 's/^last-modified: \(.\+\)\r/\1/')"
    hash="$(${bins.nix-prefetch-url} --unpack --type sha256 "${archiveUrl}/$commit.tar.gz")"

    printf '%s\n' "
    # Tracking $channel as of $(date --rfc-3339=date --date="$date").
    $setname = {
      commit = \"$commit\";
      sha256 = \"$hash\";
    };"
  }

  printSet unstableHashes nixos-unstable
  printSet stableHashes nixos-${stableRelease}
''
