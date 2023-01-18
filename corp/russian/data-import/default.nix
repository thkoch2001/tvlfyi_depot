{ depot, lib, pkgs, ... }:

let
  buildInputs = with pkgs; [
    sqlite
    pkg-config
  ];

  # mirrored input data from OpenCorpora, as of 2023-01-17.
  #
  # This data is licensed under CC-BY-SA.
  inputDataArchive = pkgs.fetchurl {
    name = "dict.opcorpora.xml.bz";
    url = "https://tazj.in/blobs/dict.opcorpora.xml.bz2";
    sha256 = "04n5g43fkfc93z6xlwf2qfdrfdfl562pc2ivdb3cbbbsy56gkqg6";
  };

  inputData = pkgs.runCommand "dict.opcorpora.xml" { } ''
    ${pkgs.bzip2}/bin/bunzip2 -k -c ${inputDataArchive} > $out
  '';

  # development shell with native deps
  shell = pkgs.mkShell {
    inherit buildInputs;

    # make OPENCORPORA_DATA available in the environment
    OPENCORPORA_DATA = inputData;
  };

in
lib.fix (self: depot.third_party.naersk.buildPackage {
  src = depot.third_party.gitignoreSource ./.;
  inherit buildInputs;

  passthru = depot.nix.readTree.drvTargets {
    inherit shell inputData;

    # target that actually builds an entire database
    database = pkgs.runCommand "tvl-russian-db.sqlite" { } ''
      ${self}/bin/data-import ${inputData} $out
    '';
  };
})
