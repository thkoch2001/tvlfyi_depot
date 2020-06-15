{ depot, ... }:

depot.buildGo.external {
  path = "github.com/pkg/browser";

  src = depot.third_party.fetchFromGitHub {
    owner = "pkg";
    repo = "browser";
    rev = "0a3d74bf9ce488f035cf5bc36f753a711bc74334";
    sha256 = "0lv6kwvm31n79mh14a63zslaf4l9bspi2q0i8i9im4njfl42iv1c";
  };
}
