{ depot, pkgs, ... }@args:

let
  inherit (import ../builder.nix args) buildGerritBazelPlugin;
in
buildGerritBazelPlugin rec {
  name = "code-owners";
  depsOutputHash = "sha256:1hd63b54zkgv8j7323inp7rdnhs2jdsb232jqlwsd9pai2f12m7n";
  src = pkgs.fetchgit {
    url = "https://gerrit.googlesource.com/plugins/code-owners";
    rev = "e654ae5bda2085bce9a99942bec440e004a114f3";
    sha256 = "sha256:14d3x3iqskgw16pvyaa0swh252agj84p9pzlf24l8lgx9d7y4biz";
  };
  patches = [
    ./using-usernames.patch
  ];
}
