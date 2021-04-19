{ depot, pkgs, ... }:

# TVL tool rust crate dependencies, where tools like carnix are not used.
# Intended for manual updates, which keeps us honest with what we pull into our closure.

let
  buildRustCrate = attrs@{
    edition ? "2018",
    ...
  }: pkgs.buildRustCrate (attrs // {
    inherit
      edition
      ;
   });
in

# TODO: remove this giant with because it screws with the static analyzer
with depot.third_party.rust-crates;

{
  cfg-if = buildRustCrate {
    pname = "cfg-if";
    crateName = "cfg-if";
    version = "1.0.0";
    sha256 = "1fzidq152hnxhg4lj6r2gv4jpnn8yivp27z6q6xy7w6v0dp6bai9";
  };

  cc = buildRustCrate {
    pname = "cc";
    crateName = "cc";
    version = "1.0.66";
    sha256 = "12q71z6ck8wlqrwgi25x3lrryyks9djymswn9b1c6qq0i01jpc1p";
  };

  ascii = buildRustCrate {
    pname = "ascii";
    crateName = "ascii";
    version = "1.0.0";
    edition = "2015";
    sha256 = "0gam8xsn981wfa40srsniivffjsfz1pg0xnigmczk9k7azb1ks1m";
  };

  regex-syntax = buildRustCrate {
    pname = "regex-syntax";
    crateName = "regex-syntax";
    version = "0.6.22";
    edition = "2015";
    sha256 = "0r00n2dgyixacl1sczqp18gxf0xh7x272hcdp62412lypba2gqyg";
  };

  regex = buildRustCrate {
    pname = "regex";
    crateName = "regex";
    version = "1.4.3";
    features = [ "std" ];
    dependencies = [ regex-syntax ];
    edition = "2015";
    sha256 = "0w0b4bh0ng20lf5y8raaxmxj46ikjqpgwy1iggzpby9lhv9vydkp";
  };

  libloading = buildRustCrate {
    pname = "libloading";
    crateName = "libloading";
    version = "0.6.7";
    dependencies = [ cfg-if ];
    edition = "2015";
    sha256 = "111d8zsizswnxiqn43vcgnc2ym9spsx1i6pcfp35ca3yw2ixq95j";
  };

  tree-sitter = buildRustCrate {
    pname = "tree_sitter";
    crateName = "tree-sitter";
    # buildRustCrate isn’t really smart enough to detect the subdir
    libPath = "binding_rust/lib.rs";
    # and the build.rs is also not where buildRustCrate would find it
    build = "binding_rust/build.rs";
    version = "0.17.1";
    dependencies = [ regex ];
    buildDependencies = [ cc ];
    sha256 = "0jwwbvs4icpra7m1ycvnyri5h3sbw4qrfvgnnvnk72h4w93qhzhr";
  };

  libc = buildRustCrate {
    pname = "libc";
    crateName = "libc";
    version = "0.2.82";
    edition = "2015";
    sha256 = "02zgn6c0xwh331hky417lbr29kmvrw3ylxs8822syyhjfjqszvsx";
  };

  bitflags = buildRustCrate {
    pname = "bitflags";
    crateName = "bitflags";
    version = "1.2.1";
    sha256 = "0b77awhpn7yaqjjibm69ginfn996azx5vkzfjj39g3wbsqs7mkxg";
  };

  inotify-sys = buildRustCrate {
    pname = "inotify-sys";
    crateName = "inotify-sys";
    version = "0.1.5";
    dependencies = [ libc ];
    sha256 = "1yiy577xxhi0j90nbg9nkd8cqwc1xix62rz55jjngvxa5jl5613v";
  };

  inotify = buildRustCrate {
    pname = "inotify";
    crateName = "inotify";
    version = "0.9.2";
    dependencies = [ bitflags libc inotify-sys ];
    sha256 = "0fcknyvknglwwk1pdzdlb4m0ry2dym1yx8r5prf2v00pxnjk0hv2";
  };

  httparse = buildRustCrate {
    pname = "httparse";
    version = "1.3.4";
    crateName = "httparse";
    edition = "2015";
    sha256 = "0dggj4s0cq69bn63q9nqzzay5acmwl33nrbhjjsh5xys8sk2x4jw";
  };

  version-check = buildRustCrate {
    pname = "version-check";
    version = "0.9.2";
    crateName = "version-check";
    edition = "2015";
    sha256 = "1vwvc1mzwv8ana9jv8z933p2xzgj1533qwwl5zr8mi89azyhq21v";
  };

  memchr = buildRustCrate {
    pname = "memchr";
    version = "2.3.3";
    crateName = "memchr";
    edition = "2015";
    sha256 = "1ivxvlswglk6wd46gadkbbsknr94gwryk6y21v64ja7x4icrpihw";
  };
  nom = buildRustCrate {
    pname = "nom";
    version = "5.1.1";
    crateName = "nom";
    sha256 = "1gb4r6mjwd645jqh02nhn60i7qkw8cgy3xq1r4clnmvz3cmkv1l0";
    dependencies = [ memchr ];
    buildDependencies = [ version-check ];
    features = [ "std" "alloc" ];
  };

  base64 = buildRustCrate {
    pname = "base64";
    version = "0.13.0";
    crateName = "base64";
    sha256 = "0i0jk5sgq37kc4c90d1g7dp7zvphbg0dbqc1ajnn0vffjxblgamg";
    features = [ "alloc" "std" ];
  };

  bufstream = buildRustCrate {
    pname = "bufstream";
    version = "0.1.4";
    crateName = "bufstream";
    sha256 = "10rqm7jly5jjx7wcc19q6q4m2zsrw3l2v3m1054wnbwvdh42xxf1";
  };

  autocfg = buildRustCrate {
    pname = "autocfg";
    version = "1.0.1";
    crateName = "autocfg";
    edition = "2015";
    sha256 = "1lsjz23jdcchcqbsmlzd4iksg3hc8bdvy677jy0938i2gp24frw1";
  };

  num-traits = buildRustCrate {
    pname = "num-traits";
    version = "0.2.14";
    crateName = "num-traits";
    edition = "2015";
    buildDependencies = [ autocfg ];
    sha256 = "09ac9dcp6cr57vjzyiy213y7312jqcy84mkamp99zr40qd1gwnyk";
  };

  num-integer = buildRustCrate {
    pname = "num-integer";
    version = "0.1.44";
    crateName = "num-integer";
    edition = "2015";
    dependencies = [ num-traits ];
    buildDependencies = [ autocfg ];
    sha256 = "1gdbnfgnivp90h644wmqj4a20yfmdga2xxxacx53pjbcazvfvajc";
  };

  chrono = buildRustCrate {
    pname = "chrono";
    version = "0.4.19";
    crateName = "chrono";
    edition = "2015";
    dependencies = [ num-traits num-integer ];
    features = [ "alloc" "std" ];
    sha256 = "0cjf5dnfbk99607vz6n5r6bhwykcypq5psihvk845sxrhnzadsar";
  };

  imap-proto = buildRustCrate {
    pname = "imap-proto";
    version = "0.10.2";
    crateName = "imap-proto";
    dependencies = [ nom ];
    sha256 = "1bf5r4d0z7c8wxrvr7kjy26500wr7cd4sxz49ix3b3yzc6ayyqv1";
  };

  lazy_static = buildRustCrate {
    pname = "lazy_static";
    version = "1.4.0";
    crateName = "lazy_static";
    sha256 = "13h6sdghdcy7vcqsm2gasfw3qg7ssa0fl3sw7lq6pdkbk52wbyfr";
  };

  imap = buildRustCrate {
    pname = "imap";
    version = "2.4.0";
    crateName = "imap";
    dependencies = [
      base64
      bufstream
      chrono
      imap-proto
      lazy_static
      nom
      regex
    ];
    sha256 = "1nj6x45qnid98nv637623rrh7imcxk0kad89ry8j5dkkgccvjyc0";
  };

  epoll = buildRustCrate {
    pname = "epoll";
    version = "4.3.1";
    crateName = "epoll";
    dependencies = [ bitflags libc ];
    sha256 = "0dgmgdmrfbjkpxn1w3xmmwsm2a623a9qdwn90s8yl78n4a36kbh9";
  };

  serde = buildRustCrate {
    pname = "serde";
    crateName = "serde";
    version = "1.0.123";
    edition = "2015";
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
  };

  log = buildRustCrate {
    pname = "log";
    version = "0.4.11";
    crateName = "log";
    sha256 = "0m6xhqxsps5mgd7r91g5mqkndbh8zbjd58p7w75r330zl4n40l07";
    dependencies = [ cfg-if ];
  };

  mustache = buildRustCrate {
    pname = "mustache";
    version = "0.9.0";
    crateName = "mustache";
    edition = "2015";
    sha256 = "1zgl8l15i19lzp90icgwyi6zqdd31b9vm8w129f41d1zd0hs7ayq";
    dependencies = [ log serde ];
  };

  semver-parser = buildRustCrate {
    pname = "semver-parser";
    version = "0.7.0";
    crateName = "semver-parser";
    edition = "2015";
    sha256 = "1da66c8413yakx0y15k8c055yna5lyb6fr0fw9318kdwkrk5k12h";
    dependencies = [ ];
  };

  semver = buildRustCrate {
    pname = "semver";
    version = "0.10.0";
    crateName = "semver";
    edition = "2015";
    sha256 = "0pbkdwlpq4d0hgdrymm2rcw31plni2siwd882gbcbscjvyvrrrqa";
    dependencies = [ semver-parser ];
  };

  toml = buildRustCrate {
    pname = "toml";
    version = "0.5.8";
    crateName = "toml";
    sha256 = "1vwjwmwsy83pbgvvm11a6grbhb09zkcrv9v95wfwv48wjm01wdj4";
    dependencies = [ serde ];
  };

  pkg-config = buildRustCrate {
    pname = "pkg-config";
    version = "0.3.19";
    crateName = "pkg_config";
    sha256 = "1kd047p8jv6mhmfzddjvfa2nwkfrb3l1wml6lfm51n1cr06cc9lz";
  };

  libz-sys = buildRustCrate {
    pname = "libz-sys";
    version = "1.1.2";
    crateName = "libz-sys";
    sha256 = "1y7v6bkwr4b6yaf951p1ns7mx47b29ziwdd5wziaic14gs1gwq30";
    buildDependencies = [
      cc
      pkg-config
    ];
  };

  libgit2-sys = buildRustCrate {
    pname = "libgit2-sys";
    version = "0.12.19+1.1.0";
    crateName = "libgit2_sys";
    sha256 = "0sw4qwkf48xh5rlv3iw3jsc8qgqkb5nhb7i8dbsyfpwxcdsd139j";
    dependencies = [
      libc
      libz-sys
    ];
    libPath = "lib.rs";
    libName = "libgit2_sys";
    # TODO: this should be available via `pkgs.defaultCrateOverrides`,
    # I thought that was included by default?
    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ pkgs.zlib pkgs.libgit2 ];
    buildDependencies = [
      cc
      pkg-config
    ];
  };

  matches = buildRustCrate {
    pname = "matches";
    version = "0.1.8";
    crateName = "matches";
    sha256 = "03hl636fg6xggy0a26200xs74amk3k9n0908rga2szn68agyz3cv";
    libPath = "lib.rs";
  };

  percent-encoding = buildRustCrate {
    pname = "percent_encoding";
    version = "2.1.0";
    crateName = "percent_encoding";
    sha256 = "0i838f2nr81585ckmfymf8l1x1vdmx6n8xqvli0lgcy60yl2axy3";
    libPath = "lib.rs";
  };

  form_urlencoded = buildRustCrate {
    pname = "form_urlencoded";
    version = "1.0.1";
    crateName = "form_urlencoded";
    sha256 = "0rhv2hfrzk2smdh27walkm66zlvccnnwrbd47fmf8jh6m420dhj8";
    dependencies = [
      matches
      percent-encoding
    ];
  };

  tinyvec_macros = buildRustCrate {
    pname = "tinyvec_macros";
    version = "0.1.0";
    crateName = "tinyvec-macros";
    sha256 = "0aim73hyq5g8b2hs9gjq2sv0xm4xzfbwp5fdyg1frljqzkapq682";
  };

  tinyvec = buildRustCrate {
    pname = "tinyvec";
    version = "1.2.0";
    crateName = "tinyvec";
    sha256 = "1c95nma20kiyrjwfsk7hzd5ir6yy4bm63fmfbfb4dm9ahnlvdp3y";
    features = [ "alloc" ];
    dependencies = [
      tinyvec_macros
    ];
  };

  unicode-normalization = buildRustCrate {
    pname = "unicode-normalization";
    version = "0.1.17";
    crateName = "unicode_normalization";
    sha256 = "0w4s0avzlf7pzcclhhih93aap613398sshm6jrxcwq0f9lhis11c";
    dependencies = [
      tinyvec
    ];
  };

  unicode-bidi = buildRustCrate {
    pname = "unicode-bidi";
    version = "0.3.5";
    crateName = "unicode_bidi";
    sha256 = "193jzlxj1dfcms2381lyd45zh4ywlicj9lzcfpid1zbkmfarymkz";
    dependencies = [
      matches
    ];
  };

  idna = buildRustCrate {
    pname = "idna";
    version = "0.2.3";
    crateName = "idna";
    sha256 = "0hwypd0fpym9lmd4bbqpwyr5lhrlvmvzhi1vy9asc5wxwkzrh299";
    dependencies = [
      matches
      unicode-normalization
      unicode-bidi
    ];
  };

  url = buildRustCrate {
    pname = "url";
    version = "2.2.1";
    crateName = "url";
    sha256 = "1ci1djafh83qhpzbmxnr9w5gcrjs3ghf8rrxdy4vklqyji6fvn5v";
    dependencies = [
      form_urlencoded
      idna
      matches
      percent-encoding
    ];
  };


  git2 = buildRustCrate {
    pname = "git2";
    version = "0.13.18";
    crateName = "git2";
    sha256 = "1z0rhlcp4fjaajjb4ldw5mdsif1328410wx7znibgnvw7lmqhv72";
    dependencies = [
      bitflags
      libc
      libgit2-sys
      log
      url
    ];
  };
}
