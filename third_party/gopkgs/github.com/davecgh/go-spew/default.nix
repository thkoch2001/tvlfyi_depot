{ depot, ... }:

depot.buildGo.external {
  path = "github.com/davecgh/go-spew";
  src = depot.third_party.fetchFromGitHub {
    owner = "davecgh";
    repo = "go-spew";
    rev = "8991bc29aa16c548c550c7ff78260e27b9ab7c73";
    sha256 = "0hka6hmyvp701adzag2g26cxdj47g21x6jz4sc6jjz1mn59d474y";
  };
}
