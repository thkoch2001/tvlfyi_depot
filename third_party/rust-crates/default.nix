{ depot, pkgs, ... }:

# TVL tool rust crate dependencies, where tools like carnix are not used.
# Intended for manual updates, which keeps us honest with what we pull into our closure.

let
  buildRustCrate = attrs@{
    edition ? "2018",
    pname,
    crateName ? pname,
    ...
  }: pkgs.buildRustCrate (attrs // {
    inherit
      crateName
      edition
      ;
   });

  R = depot.third_party.rust-crates;
in

{
  cfg-if = buildRustCrate {
    pname = "cfg-if";
    version = "1.0.0";
    sha256 = "1fzidq152hnxhg4lj6r2gv4jpnn8yivp27z6q6xy7w6v0dp6bai9";
  };

  cc = buildRustCrate {
    pname = "cc";
    version = "1.0.66";
    sha256 = "12q71z6ck8wlqrwgi25x3lrryyks9djymswn9b1c6qq0i01jpc1p";
  };

  ascii = buildRustCrate {
    pname = "ascii";
    version = "1.0.0";
    edition = "2015";
    sha256 = "0gam8xsn981wfa40srsniivffjsfz1pg0xnigmczk9k7azb1ks1m";
  };

  regex-syntax = buildRustCrate {
    pname = "regex-syntax";
    version = "0.6.22";
    edition = "2015";
    sha256 = "0r00n2dgyixacl1sczqp18gxf0xh7x272hcdp62412lypba2gqyg";
  };

  regex = buildRustCrate {
    pname = "regex";
    version = "1.4.3";
    features = [ "std" ];
    dependencies = [ R.regex-syntax ];
    edition = "2015";
    sha256 = "0w0b4bh0ng20lf5y8raaxmxj46ikjqpgwy1iggzpby9lhv9vydkp";
  };

  libloading = buildRustCrate {
    pname = "libloading";
    version = "0.6.7";
    dependencies = [ R.cfg-if ];
    edition = "2015";
    sha256 = "111d8zsizswnxiqn43vcgnc2ym9spsx1i6pcfp35ca3yw2ixq95j";
  };

  tree-sitter = buildRustCrate {
    pname = "tree-sitter";
    # buildRustCrate isn’t really smart enough to detect the subdir
    libPath = "binding_rust/lib.rs";
    # and the build.rs is also not where buildRustCrate would find it
    build = "binding_rust/build.rs";
    version = "0.17.1";
    dependencies = [ R.regex ];
    buildDependencies = [ R.cc ];
    sha256 = "0jwwbvs4icpra7m1ycvnyri5h3sbw4qrfvgnnvnk72h4w93qhzhr";
  };

  libc = buildRustCrate {
    pname = "libc";
    version = "0.2.82";
    edition = "2015";
    sha256 = "02zgn6c0xwh331hky417lbr29kmvrw3ylxs8822syyhjfjqszvsx";
  };

  bitflags = buildRustCrate {
    pname = "bitflags";
    version = "1.2.1";
    sha256 = "0b77awhpn7yaqjjibm69ginfn996azx5vkzfjj39g3wbsqs7mkxg";
  };

  inotify-sys = buildRustCrate {
    pname = "inotify-sys";
    version = "0.1.5";
    dependencies = [ R.libc ];
    sha256 = "1yiy577xxhi0j90nbg9nkd8cqwc1xix62rz55jjngvxa5jl5613v";
  };

  inotify = buildRustCrate {
    pname = "inotify";
    version = "0.9.2";
    dependencies = [ R.bitflags R.libc R.inotify-sys ];
    sha256 = "0fcknyvknglwwk1pdzdlb4m0ry2dym1yx8r5prf2v00pxnjk0hv2";
  };

  httparse = buildRustCrate {
    pname = "httparse";
    version = "1.3.4";
    edition = "2015";
    sha256 = "0dggj4s0cq69bn63q9nqzzay5acmwl33nrbhjjsh5xys8sk2x4jw";
  };

  version-check = buildRustCrate {
    pname = "version-check";
    version = "0.9.2";
    edition = "2015";
    sha256 = "1vwvc1mzwv8ana9jv8z933p2xzgj1533qwwl5zr8mi89azyhq21v";
  };

  memchr = buildRustCrate {
    pname = "memchr";
    version = "2.3.3";
    edition = "2015";
    sha256 = "1ivxvlswglk6wd46gadkbbsknr94gwryk6y21v64ja7x4icrpihw";
  };
  nom = buildRustCrate {
    pname = "nom";
    version = "5.1.1";
    sha256 = "1gb4r6mjwd645jqh02nhn60i7qkw8cgy3xq1r4clnmvz3cmkv1l0";
    dependencies = [ R.memchr ];
    buildDependencies = [ R.version-check ];
    features = [ "std" "alloc" ];
  };

  base64 = buildRustCrate {
    pname = "base64";
    version = "0.13.0";
    sha256 = "0i0jk5sgq37kc4c90d1g7dp7zvphbg0dbqc1ajnn0vffjxblgamg";
    features = [ "alloc" "std" ];
  };

  bufstream = buildRustCrate {
    pname = "bufstream";
    version = "0.1.4";
    sha256 = "10rqm7jly5jjx7wcc19q6q4m2zsrw3l2v3m1054wnbwvdh42xxf1";
  };

  autocfg = buildRustCrate {
    pname = "autocfg";
    version = "1.0.1";
    edition = "2015";
    sha256 = "1lsjz23jdcchcqbsmlzd4iksg3hc8bdvy677jy0938i2gp24frw1";
  };

  num-traits = buildRustCrate {
    pname = "num-traits";
    version = "0.2.14";
    edition = "2015";
    buildDependencies = [ R.autocfg ];
    sha256 = "09ac9dcp6cr57vjzyiy213y7312jqcy84mkamp99zr40qd1gwnyk";
  };

  num-integer = buildRustCrate {
    pname = "num-integer";
    version = "0.1.44";
    edition = "2015";
    dependencies = [ R.num-traits ];
    buildDependencies = [ R.autocfg ];
    sha256 = "1gdbnfgnivp90h644wmqj4a20yfmdga2xxxacx53pjbcazvfvajc";
  };

  chrono = buildRustCrate {
    pname = "chrono";
    version = "0.4.19";
    edition = "2015";
    dependencies = [ R.num-traits R.num-integer ];
    features = [ "alloc" "std" ];
    sha256 = "0cjf5dnfbk99607vz6n5r6bhwykcypq5psihvk845sxrhnzadsar";
  };

  imap-proto = buildRustCrate {
    pname = "imap-proto";
    version = "0.10.2";
    dependencies = [ R.nom ];
    sha256 = "1bf5r4d0z7c8wxrvr7kjy26500wr7cd4sxz49ix3b3yzc6ayyqv1";
  };

  lazy_static = buildRustCrate {
    pname = "lazy_static";
    version = "1.4.0";
    sha256 = "13h6sdghdcy7vcqsm2gasfw3qg7ssa0fl3sw7lq6pdkbk52wbyfr";
  };

  imap = buildRustCrate {
    pname = "imap";
    version = "2.4.0";
    dependencies = [
      R.base64
      R.bufstream
      R.chrono
      R.imap-proto
      R.lazy_static
      R.nom
      R.regex
    ];
    sha256 = "1nj6x45qnid98nv637623rrh7imcxk0kad89ry8j5dkkgccvjyc0";
  };

  epoll = buildRustCrate {
    pname = "epoll";
    version = "4.3.1";
    dependencies = [ R.bitflags R.libc ];
    sha256 = "0dgmgdmrfbjkpxn1w3xmmwsm2a623a9qdwn90s8yl78n4a36kbh9";
  };

  serde = buildRustCrate {
    pname = "serde";
    version = "1.0.123";
    edition = "2015";
    sha256 = "05xl2s1vpf3p7fi2yc9qlzw88d5ap0z3qmhmd7axa6pp9pn1s5xc";
    features = [ "std" ];
  };

  ryu = buildRustCrate {
    pname = "ryu";
    version = "1.0.5";
    sha256 = "060y2ln1csix593ingwxr2y3wl236ls0ly1ffkv39h5im7xydhrc";
  };

  itoa = buildRustCrate {
    pname = "itoa";
    version = "0.4.7";
    sha256 = "0079jlkcmcaw37wljrvb6r3dqq15nfahkqnl5npvlpdvkg31k11x";
  };

  serde_json = buildRustCrate {
    pname = "serde_json";
    version = "1.0.62";
    sha256 = "0sgc8dycigq0nxr4j613m4q733alfb2i10s6nz80lsbbqgrka21q";
    dependencies = [ R.serde R.ryu R.itoa ];
    features = [ "std" ];
  };

  log = buildRustCrate {
    pname = "log";
    version = "0.4.11";
    sha256 = "0m6xhqxsps5mgd7r91g5mqkndbh8zbjd58p7w75r330zl4n40l07";
    dependencies = [ R.cfg-if ];
  };

  mustache = buildRustCrate {
    pname = "mustache";
    version = "0.9.0";
    edition = "2015";
    sha256 = "1zgl8l15i19lzp90icgwyi6zqdd31b9vm8w129f41d1zd0hs7ayq";
    dependencies = [ R.log R.serde ];
  };

  semver-parser = buildRustCrate {
    pname = "semver-parser";
    version = "0.7.0";
    edition = "2015";
    sha256 = "1da66c8413yakx0y15k8c055yna5lyb6fr0fw9318kdwkrk5k12h";
  };

  semver = buildRustCrate {
    pname = "semver";
    version = "0.10.0";
    edition = "2015";
    sha256 = "0pbkdwlpq4d0hgdrymm2rcw31plni2siwd882gbcbscjvyvrrrqa";
    dependencies = [ R.semver-parser ];
  };

  toml = buildRustCrate {
    pname = "toml";
    version = "0.5.8";
    sha256 = "1vwjwmwsy83pbgvvm11a6grbhb09zkcrv9v95wfwv48wjm01wdj4";
    edition = "2018";
    dependencies = [ R.serde ];
  };
}
