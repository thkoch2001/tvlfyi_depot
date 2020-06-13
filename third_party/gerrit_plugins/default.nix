{ pkgs, ... }:

let inherit (pkgs) fetchurl;
in {
  # https://gerrit.googlesource.com/plugins/owners
  owners = fetchurl {
    url = "https://storage.googleapis.com/tazjins-data/tvl/owners_3.1.jar";
    sha256 = "1kp61hbz74irsxsbgrj9hk8jlavjx8jvkmbf3kwfa62wvzivc94b";
  } // { name = "owners"; };

  # https://gerrit.googlesource.com/plugins/owners
  owners-autoassign = fetchurl {
    url = "https://storage.googleapis.com/tazjins-data/tvl/owners-autoassign_3.1.jar";
    sha256 = "1kfrgbds62jx1bxwwnzmd6jrb1sg671kssk7sy4nkyb6p5vlq8hq";
  } // { name = "owners-autoassign"; };
}
