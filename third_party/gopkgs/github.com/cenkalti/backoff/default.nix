{ depot, ... }:

depot.buildGo.external {
  path = "github.com/cenkalti/backoff/v4";

  src = depot.third_party.fetchFromGitHub {
    owner = "cenkalti";
    repo = "backoff";
    rev = "18fe4ce5a8550e0d0919b680ad3c080a5455bddf";
    sha256 = "083617p066p77ik0js8wwgb5qzabgvl8wqpkjb8s9alpyqsq2mpg";
  };
}
