{ pkgs, ... }:

let inherit (pkgs) fetchurl;
in {
  # https://gerrit.googlesource.com/plugins/owners
  owners = fetchurl {
    url = "https://storage.googleapis.com/tazjins-data/tvl/owners_3.2.jar";
    sha256 = "1xw1q3g0353aw5jqxp69n85f8y57l2p51np37n8r34kzbn5r4iz7";
  } // { name = "owners"; };

  # https://gerrit.googlesource.com/plugins/owners
  owners-autoassign = fetchurl {
    url = "https://storage.googleapis.com/tazjins-data/tvl/owners-autoassign_3.2.jar";
    sha256 = "1yr6rpzlac2kdxagjk8mfw6fj66fsn31v5h03bi129ri29ikppql";
  } // { name = "owners-autoassign"; };
}
