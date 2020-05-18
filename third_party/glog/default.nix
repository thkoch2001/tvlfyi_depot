{ pkgs, ... }:

pkgs.originals.glog.overrideAttrs(old: {
  version = "master-20200518";

  # packaged version has a patch that is now in master.
  patches = [];

  src = pkgs.fetchFromGitHub {
    owner = "google";
    repo = "glog";
    rev = "0a2e5931bd5ff22fd3bf8999eb8ce776f159cda6";
    sha256 = "08yih0hc63j02p0ms4ydbgf2c13v2c7knjzp3qdin0v3sng896wg";
  };
})
