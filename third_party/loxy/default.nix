{ lib, pkgs, depot, ... }:

let

  inherit (depot.nix) fetchGoModule;

in

pkgs.buildGoModule rec {
  pname = "loxy";
  version = "0.1.1";

  src = fetchGoModule {
    path = "go.anomalous.eu/loxy";
    inherit version;
    sha256 = "0k1i7wa0v9h7r4kb7pjd3pzvcjrnsp58cfh51z73s74il7462sr5";
  };

  vendorSha256 = "0243wq6crzy86vc5cm1nijddsrcllr3c66xkd6q8x16jrvnv09jx";

  postInstall = ''
    install -D -m 0644 -t $out/share/man/man8 loxy.8
  '';

  meta = with lib; {
    description = "a logging IRC proxy";
    homepage = "https://anomalous.eu/projects/loxy";
    vcs = "https://src.anomalous.eu/loxy";
    license = licenses.gpl3Plus;
    maintainers = with maintainers; [ V edef ];
  };
}
