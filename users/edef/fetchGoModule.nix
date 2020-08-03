{ pkgs, ... }:

{ path, version, sha256 }:

(pkgs.fetchurl {
  name = "source";
  url = "https://proxy.golang.org/${path}/@v/v${version}.zip";
  inherit sha256;

  recursiveHash = true;
  downloadToTemp = true;

  postFetch = ''
    unpackDir="$TMPDIR/unpack"
    mkdir "$unpackDir"
    cd "$unpackDir"

    mv "$downloadedFile" "$TMPDIR/src.zip"
    unpackFile "$TMPDIR/src.zip"
    mv "$unpackDir/${path}@v${version}" "$out"
  '';
}).overrideAttrs ({ nativeBuildInputs ? [], ... }: {
  nativeBuildInputs = nativeBuildInputs ++ [ pkgs.unzip ];
})
