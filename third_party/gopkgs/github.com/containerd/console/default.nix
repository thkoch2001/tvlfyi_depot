{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/containerd/console";
  src = pkgs.fetchFromGitHub {
    owner = "containerd";
    repo = "console";
    rev = "v1.0.1";
    sha256 = "0s837wj6h80fykk2pdmaji75rw9c3863by0gh0cq51hh0lgyjpvg";
  };

  deps = with depot.third_party; [ gopkgs."golang.org".x.sys.unix ];
}
