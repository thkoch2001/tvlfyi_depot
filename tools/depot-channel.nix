{ depot, pkgs, ... }:

let
  bins = depot.nix.getBins pkgs.git [
    "git"
  ] // depot.nix.getBins pkgs.coreutils [
    "mkdir"
    "mktemp"
    "printf"
    "test"
  ] // depot.nix.getBins pkgs.xz [
    "xz"
  ] // depot.nix.getBins pkgs.gnutar [
    "tar"
  ] // depot.nix.getBins pkgs.nix [
    "nix-instantiate"
  ];
in

pkgs.writers.writeBashBin "depot-channel" ''
  set -euo pipefail

  usage() {
    echo "Usage: $0 [--help] CHANNEL_DIR" >&2
    exit "$1" # TODO
  }

  if ${bins.test} "$#" -eq 0; then
    usage 101
  fi

  for arg in "$@"; do
    case "$arg" in
      -h|--help|--usage|-?)
        usage 0
        ;;
      -*)
        echo "$0: Unknown flag $arg" >&2
        exit 101 # TODO
        ;;
      *)
       if test -z "''${channelDir:-}"; then
         channelDir="$arg"
       else
         echo "$0: Unexpected argument: $arg" >&2
       fi
       ;;
     esac
  done

  tmp="$(${bins.mktemp} --suffix=depot-channel.tar)"

  cleanup() {
    rm -f "$tmp"
  }

  trap cleanup EXIT

  r="$(${bins.git} name-rev --refs='refs/r/*' HEAD | cut -d' ' -f2)"

  if ${bins.test} -n "''${DEPOT_CHANNEL_REVISION:-}"; then
    r="$DEPOT_CHANNEL_REVISION"
  else
    if ${bins.test} -z "$r" -o "$r" = "undefined"; then
      echo "$0: HEAD has no depot revision. Are you using upstream canon?" >&2
      exit 1 # TODO
    fi
  fi

  echo "Creating channel tarball for depot at $r" >&2

  prefix="depot-$(${bins.printf} %s "$r" | tr / .)"
  depotPath="$(${bins.git} rev-parse --show-toplevel)"
  nixpkgs="$(${bins.nix-instantiate} --eval --read-write-mode \
    -A third_party.nixpkgs.path "$depotPath")"
  target="$channelDir/$r/nixexprs.tar.xz"

  ${bins.git} archive -o "$tmp" --format=tar --prefix="$prefix/" "$r"

  ${bins.tar} -rPf "$tmp" "$nixpkgs" \
    --transform "s|^$nixpkgs|$prefix/third_party/nixpkgs/unstable|" \
    --mode 'u+rw,a+rX'

  ${bins.mkdir} -p "$(dirname "$target")"
  ${bins.xz} -z < "$tmp" > "$target"
''
