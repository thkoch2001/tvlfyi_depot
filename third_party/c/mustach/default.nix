{ depot, pkgs, ... }:

let
  version = "0.99";
  src = pkgs.fetchFromGitLab {
    owner = "jobol";
    repo = "mustach";
    rev = version;
    sha256 = "1zffwi43gwjam30n3c94i61zf708zp9shj6zrm0309d13l1dnx1w";
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
