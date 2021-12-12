{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/mattn/go-isatty";
  src = pkgs.fetchFromGitHub {
    owner = "mattn";
    repo = "go-isatty";
    rev = "v0.0.12";
    sha256 = "1dfsh27d52wmz0nmmzm2382pfrs2fcijvh6cgir7jbb4pnigr5w4";
  };

  deps = with depot.third_party; [ gopkgs."golang.org".x.sys.unix ];
}
