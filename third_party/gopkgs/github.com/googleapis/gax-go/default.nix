{ depot, pkgs, ... }:

depot.nix.buildGo.external {
  path = "github.com/googleapis/gax-go";

  src = pkgs.fetchFromGitHub {
    owner = "googleapis";
    repo = "gax-go";
    rev = "b443e5a67ec8eeac76f5f384004931878cab24b3";
    sha256 = "075s8b76l14c9vlchly38hsf28bnr7vzq9q57g2kg1025h004lzw";
  };

  deps = with depot.third_party; [
    gopkgs."golang.org".x.net.trace.gopkg
    gopkgs."google.golang.org".grpc.gopkg
    gopkgs."google.golang.org".grpc.codes.gopkg
    gopkgs."google.golang.org".grpc.status.gopkg
  ];
}
