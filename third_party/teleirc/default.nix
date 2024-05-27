{ pkgs, lib, ... }:

pkgs.buildGoModule rec {
  name = "teleirc";
  version = "2.3.0-4";

  src = pkgs.fetchFromGitHub {
    owner = "tvlfyi";
    repo = "teleirc";
    rev = "51aaf11a0a1c8d86fadf50db44215b06555e3c9b";
    sha256 = "09lr7zs93w5ipr4l820g40jj0q5j9y6mqwwhwry8pc99zbqchjm0";
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
