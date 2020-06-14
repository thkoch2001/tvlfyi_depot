{ depot, ... }:

depot.buildGo.external {
  path = "gopkg.in/irc.v3";

  src = depot.third_party.fetchFromGitHub {
    owner = "go-irc";
    repo = "irc";
    rev = "21a5301d6035ea204b2a7bb522a7b4598e5f6b28";
    sha256 = "1pi5y73pr4prhw5bvmp4babiw02nndizgmpksdgrrg28l9f2wm0n";
  };
}
