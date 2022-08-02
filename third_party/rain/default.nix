{ pkgs, ... }:

let
  inherit (pkgs) buildGoModule fetchFromGitHub;
in
buildGoModule rec {
  pname = "rain";
  version = "1.2.0";

  src = fetchFromGitHub {
    owner = "aws-cloudformation";
    repo = pname;
    rev = "v${version}";
    sha256 = "168gkchshl5f1awqi1cgvdkm6q707702rnn0v4i5djqxmg5rk0p9";
  };

  vendorSha256 = "16bx7cjh5cq9zlis8lf28i016avgqf3j9fmcvkqzd8db2vxpqx3v";
}
