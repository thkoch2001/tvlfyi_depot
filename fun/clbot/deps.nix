{ depot, ... }:

let
  inherit (depot.nix.buildGo) external;
in
{
  backoff = external {
    path = "github.com/cenkalti/backoff/v4";
    src = fetchGit {
      url = "https://github.com/cenkalti/backoff";
      rev = "18fe4ce5a8550e0d0919b680ad3c080a5455bddf";
    };
  };
  spew = external {
    path = "github.com/davecgh/go-spew";
    src = fetchGit {
      url = "https://github.com/davecgh/go-spew";
      rev = "8991bc29aa16c548c550c7ff78260e27b9ab7c73";
    };
  };
  glog = external {
    path = "github.com/golang/glog";
    src = fetchGit {
      url = "https://github.com/golang/glog";
      rev = "23def4e6c14b4da8ac2ed8007337bc5eb5007998";
    };
  };
  go-cmp = external {
    path = "github.com/google/go-cmp";
    src = fetchGit {
      url = "https://github.com/google/go-cmp";
      rev = "7e5cb83929c528b29e5a8ac1244eab0436f79bce";
    };
  };
  x-crypto = external {
    path = "golang.org/x/crypto";
    src = fetchGit {
      url = "https://github.com/golang/crypto";
      rev = "70a84ac30bf957c7df57edd1935d2081871515e1";
    };
  };
}
