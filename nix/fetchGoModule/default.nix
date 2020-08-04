{ lib, pkgs, ... }:

let

  inherit (lib)
    lowerChars
    replaceStrings
    upperChars
  ;

  caseFold = replaceStrings upperChars (map (c: "!" + c) lowerChars);

in

{ path, version, sha256 }:

pkgs.fetchurl {
  name = "source";
  url = "https://proxy.golang.org/${caseFold path}/@v/v${version}.zip";
  inherit sha256;

  recursiveHash = true;
  downloadToTemp = true;

  postFetch = ''
    unpackDir="$TMPDIR/unpack"
    mkdir "$unpackDir"
    cd "$unpackDir"

    mv "$downloadedFile" "$TMPDIR/src.zip"
    ${pkgs.unzip}/bin/unzip "$TMPDIR/src.zip" -d "$unpackDir"
    mv "$unpackDir/${path}@v${version}" "$out"
  '';
}
