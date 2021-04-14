# mustach is a library for rendering mustache(5) templates in C.
# This derivation packages the dependency-free core library, but
# not the JSON-integrations or glue code.
{ depot, pkgs, ... }:

let
  version = "1.1.0";
  src = pkgs.fetchFromGitLab {
    owner = "jobol";
    repo = "mustach";
    rev = version;
    sha256 = "1y9xy0hxwjyrk872g9i07j25xhcnjmmkjwmncjba9ppwsapp96ia";
  };

  file = f: src + "/${f}";
in
  depot.nix.buildC.library {
    name = "mustach-${version}";
    libName = "mustach";

    include = [
      (file "mustach.h")
    ];

    srcs = [
      (file "mustach.c")
    ];
  }
