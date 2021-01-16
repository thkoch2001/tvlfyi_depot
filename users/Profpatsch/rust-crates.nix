{ depot, pkgs, ... }:
rec {
  cfg-if = pkgs.buildRustCrate {
    pname = "cfg-if";
    crateName = "cfg-if";
    version = "1.0.0";
    sha256 = "1fzidq152hnxhg4lj6r2gv4jpnn8yivp27z6q6xy7w6v0dp6bai9";
  };

  cc = pkgs.buildRustCrate {
    pname = "cc";
    crateName = "cc";
    version = "1.0.66";
    sha256 = "12q71z6ck8wlqrwgi25x3lrryyks9djymswn9b1c6qq0i01jpc1p";
  };

  regex-syntax = pkgs.buildRustCrate {
    pname = "regex-syntax";
    crateName = "regex-syntax";
    version = "0.6.22";
    sha256 = "0r00n2dgyixacl1sczqp18gxf0xh7x272hcdp62412lypba2gqyg";
  };

  regex = pkgs.buildRustCrate {
    pname = "regex";
    crateName = "regex";
    version = "1.4.3";
    features = [ "std" ];
    dependencies = [ regex-syntax ];
    sha256 = "0w0b4bh0ng20lf5y8raaxmxj46ikjqpgwy1iggzpby9lhv9vydkp";
  };

  libloading = pkgs.buildRustCrate {
    pname = "libloading";
    crateName = "libloading";
    version = "0.6.7";
    dependencies = [ cfg-if ];
    sha256 = "111d8zsizswnxiqn43vcgnc2ym9spsx1i6pcfp35ca3yw2ixq95j";
  };

  tree-sitter = pkgs.buildRustCrate {
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
}
