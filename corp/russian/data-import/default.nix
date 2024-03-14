{
  depot,
  lib,
  pkgs,
  ...
}:

let
  buildInputs = with pkgs; [
    sqlite
    pkg-config
  ];

  # mirrored input data from OpenCorpora, as of 2023-01-17.
  #
  # This data is licensed under CC-BY-SA.
  openCorporaArchive = pkgs.fetchurl {
    name = "dict.opcorpora.xml.bz";
    url = "https://tazj.in/blobs/opencorpora-20230117.xml.bz2";
    sha256 = "04n5g43fkfc93z6xlwf2qfdrfdfl562pc2ivdb3cbbbsy56gkqg6";
  };

  openCorpora = pkgs.runCommand "dict.opcorpora.xml" { } ''
    ${pkgs.bzip2}/bin/bunzip2 -k -c ${openCorporaArchive} > $out
  '';

  # mirrored input data from OpenRussian, as of 2023-01-17.
  #
  # This data is licensed under CC-BY-SA.
  openRussianArchive = pkgs.fetchzip {
    name = "openrussian-20230117";
    url = "https://tazj.in/blobs/openrussian-20230117.tar.xz";
    sha256 = "06jl7i23cx58a0n2626hb82xlzimixvnxp7lxdw0g664kv9bmw25";
  };

  # development shell with native deps
  shell = pkgs.mkShell {
    inherit buildInputs;

    # make datasets available in the environment
    OPENCORPORA_DATA = openCorpora;
    OPENRUSSIAN_DATA = openRussianArchive;
  };
in
lib.fix (
  self:
  depot.third_party.naersk.buildPackage {
    src = depot.third_party.gitignoreSource ./.;
    inherit buildInputs;

    passthru = depot.nix.readTree.drvTargets {
      inherit shell openCorpora;

      # target that actually builds an entire database
      database = pkgs.runCommand "tvl-russian-db.sqlite" {
        OPENCORPORA_DATA = openCorpora;
        OPENRUSSIAN_DATA = openRussianArchive;
      } "${self}/bin/data-import --output $out";
    };
  }
)
