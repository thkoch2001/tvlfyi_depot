{ depot, pkgs, ... }:

let
  inherit (depot.users.Profpatsch.writers)
    rustSimpleBin
    ;

  inherit (pkgs)
    buildRustCrate
    ;

  serde = buildRustCrate {
    pname = "serde";
    crateName = "serde";
    version = "1.0.123";
    sha256 = "05xl2s1vpf3p7fi2yc9qlzw88d5ap0z3qmhmd7axa6pp9pn1s5xc";
    features = [ "std" ];
  };

  ryu = buildRustCrate {
    pname = "ryu";
    version = "1.0.5";
    crateName = "ryu";
    sha256 = "060y2ln1csix593ingwxr2y3wl236ls0ly1ffkv39h5im7xydhrc";
  };

  itoa = buildRustCrate {
    pname = "itoa";
    version = "0.4.7";
    crateName = "itoa";
    sha256 = "0079jlkcmcaw37wljrvb6r3dqq15nfahkqnl5npvlpdvkg31k11x";
  };

  serde_json = buildRustCrate {
    pname = "serde_json";
    version = "1.0.62";
    crateName = "serde_json";
    sha256 = "0sgc8dycigq0nxr4j613m4q733alfb2i10s6nz80lsbbqgrka21q";
    dependencies = [ serde ryu itoa ];
    features = [ "std" ];
    edition = "2018";
  };

in

  rustSimpleBin {
    name = "ncla-run";
    dependencies = [ serde_json ];
  } (builtins.readFile ./ncla-run.rs)
