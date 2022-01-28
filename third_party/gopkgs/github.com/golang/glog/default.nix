{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "github.com/golang/glog";
    src =
      pkgs.fetchFromGitHub
        {
          owner = "golang";
          repo = "glog";
          rev = "23def4e6c14b4da8ac2ed8007337bc5eb5007998";
          sha256 = "0jb2834rw5sykfr937fxi8hxi2zy80sj2bdn9b3jb4b26ksqng30";
        };
  }
