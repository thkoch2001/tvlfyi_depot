# https://github.com/irccloud/irccat
{ lib, pkgs, ... }:

pkgs.buildGoModule rec {
  pname = "irccat";
  version = "20201108";
  meta.license = lib.licenses.gpl3;
  vendorSha256 = "06a985y4alw1rsghgmhfyczns6klz7bbkfn5mnqc9fdfclgg4s3r";

  src = pkgs.fetchFromGitHub {
    owner = "irccloud";
    repo = "irccat";
    rev = "17451e7e267f099e9614ec945541b624520f607e";
    sha256 = "0l99mycxymyslwi8mmyfdcqa8pdp79wcyb04s5j5y4grmlsxw1wx";
  };
}
