{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "golang.org/x/oauth2";
    src =
      pkgs.fetchgit
        {
          url = "https://go.googlesource.com/oauth2";
          rev = "858c2ad4c8b6c5d10852cb89079f6ca1c7309787";
          hash = "sha256:1dc7n8ddph8w6q0i3cwlgvjwpf2wlkx407va1ydnazasi1j5ixrw";
        };
    deps =
      with depot.third_party;
      [ gopkgs."golang.org".x.net.context.ctxhttp gopkgs."cloud.google.com".go.compute.metadata ];
  }
