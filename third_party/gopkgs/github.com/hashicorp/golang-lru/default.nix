{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "github.com/hashicorp/golang-lru";
    src =
      pkgs.fetchgit
        {
          url = "https://github.com/hashicorp/golang-lru";
          rev = "7f827b33c0f158ec5dfbba01bb0b14a4541fd81d";
          hash = "sha256:1p2igd58xkm8yaj2c2wxiplkf2hj6kxwrg6ss7mx61s5rd71v5xb";
        };
    deps =
      with depot.third_party;
      [ gopkgs."golang.org".x.net.context.ctxhttp gopkgs."cloud.google.com".go.compute.metadata ];
  }
