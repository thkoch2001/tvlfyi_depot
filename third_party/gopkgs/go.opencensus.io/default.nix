{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "go.opencensus.io";
    src =
      pkgs.fetchFromGitHub
        {
          owner = "census-instrumentation";
          repo = "opencensus-go";
          rev = "643eada29081047b355cfaa1ceb9bc307a10423c";
          sha256 = "1acmv2f5wz06abphk0yvb9igp2j5sn1v21dg1p8n109rwanwd5v4";
        };
    deps =
      with depot.third_party;
      [ gopkgs."github.com".hashicorp.golang-lru.simplelru gopkgs."github.com".golang.groupcache.lru ];
  }
