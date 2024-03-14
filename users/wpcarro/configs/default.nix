{ pkgs, ... }:

let
  inherit (pkgs) writeShellScript;
  inherit (pkgs.lib.strings) makeBinPath;
in
{
  install = writeShellScript "install-configs" ''
    cd "$WPCARRO/configs" && ${pkgs.stow}/bin/stow --target="$HOME" .
  '';

  uninstall = writeShellScript "uninstall-configs" ''
    cd "$WPCARRO/configs" && ${pkgs.stow}/bin/stow --delete --target="$HOME" .
  '';

  # Run this script to import all of the information exported by `export.sh`.
  # Usage: import-gpg path/to/export.zip
  import-gpg = writeShellScript "import-gpg" ''
    set -euo pipefail

    if [ -z "''${1+x}" ]; then
      echo "You must specify the path to export.zip. Exiting..."
      exit 1
    fi

    PATH="${
      makeBinPath (
        with pkgs;
        [
          busybox
          gnupg
        ]
      )
    }"
    destination="$(mktemp -d)"

    function cleanup() {
      rm -rf "$destination"
    }
    trap cleanup EXIT

    unzip "$1" -d "$destination" >/dev/null

    gpg --import "$destination/public.asc"
    gpg --import "$destination/secret.asc"
    gpg --import-ownertrust "$destination/ownertrust.txt"

    # Run this at the end to output some verification
    gpg --list-keys
    gpg --list-secret-keys
  '';

  # Run this script to export all the information required to transport your GPG
  # information to a zip file.
  # Usage: export-gpg
  export-gpg = writeShellScript "export-gpg" ''
    set -euo pipefail

    PATH="${
      makeBinPath (
        with pkgs;
        [
          busybox
          gnupg
          zip
        ]
      )
    }"
    output="$(pwd)/export.zip"
    destination="$(mktemp -d)"

    function cleanup() {
      rm -rf "$destination"
    }
    trap cleanup EXIT

    gpg --armor --export >"$destination/public.asc"
    gpg --armor --export-secret-keys >"$destination/secret.asc"
    gpg --armor --export-ownertrust >"$destination/ownertrust.txt"

    # Strangely enough this appears to be the only way to create a zip of a
    # directory that doesn't contain the (noisy) full paths of each item from
    # the source filesystem. (i.e. -j doesn't cooperate with -r).
    pushd "$destination"
    zip -r "$output" ./*
    popd

    echo "$(realpath $output)"
  '';
}
