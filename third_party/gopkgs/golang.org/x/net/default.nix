{ depot
, pkgs
, ...
}:
depot.nix.buildGo.external
  {
    path = "golang.org/x/net";
    src =
      pkgs.fetchgit
        {
          url = "https://go.googlesource.com/net";
          rev = "c0dbc17a35534bf2e581d7a942408dc936316da4";
          hash = "sha256:1f1xqh2cvr629fkg9n9k347vf6g91jkrsmgmy8hlqdrq163blb54";
        };
    deps =
      with depot.third_party;
      [
        gopkgs."golang.org".x.text.secure.bidirule.gopkg
        gopkgs."golang.org".x.text.unicode.bidi.gopkg
        gopkgs."golang.org".x.text.unicode.norm.gopkg
      ];
  }
