{ pkgs, ... }:

pkgs.buildGoPackage {
  name = "hound";
  goPackagePath = "github.com/hound-search/hound";
  subPackages = ["cmds/hound" "cmds/houndd"];

  src = pkgs.fetchFromGitHub {
    owner = "hound-search";
    repo = "hound";
    rev = "ac0241d63ea15fe7fd2bfff4d8217f18ea589e92";
    sha256 = "00ikhwiazdznc8njc1df8g768q28qx6y4885mi4lff15mmja8x64";
  };
}
