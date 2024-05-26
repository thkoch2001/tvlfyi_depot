{ pkgs, lib, ... }:

pkgs.buildGoModule rec {
  name = "teleirc";
  version = "2.3.0-4";

  src = pkgs.fetchFromGitHub {
    owner = "RITlug";
    repo = "teleirc";
    rev = "v2.3.0-4-g9ad5f9a";
    sha256 = "08h9ady2vz6nfcii2ac41b2lfqzjfiwv8apbcx4fybaymwy5vn7g";
  };

  vendorHash = "sha256:06f2wyxbphj73wknpp6dsn7rb4yhvdl6x0gj729cns7r4bsviscs";
  ldflags = [ "-s" "-w" "-X" "main.version=${version}" ];
  postInstall = "mv $out/bin/cmd $out/bin/teleirc";

  meta = with lib; {
    description = "IRC/Telegram bridge";
    homepage = "https://docs.teleirc.com/en/latest/";
    license = licenses.gpl3;
  };
}
