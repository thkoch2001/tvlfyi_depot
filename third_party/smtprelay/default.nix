# A simple SMTP relay without the kitchen sink.
{ pkgs, lib, ... }:

pkgs.buildGoModule {
  name = "smtprelay";
  vendorSha256 = "0kv9cv2jca2r90qsf40qmqpw84kgxvbxlf39bfw8rvs2lnmqc2dg";

  src = pkgs.fetchFromGitHub {
    owner = "decke";
    repo = "smtprelay";
    rev = "ed1c3a98889e752291aaca6c64149e48452d0583";
    sha256 = "16q2d2ja2cipjvsnfxmdzixkg85sh15rh9r95w6bw2r1gjqr65hr";
  };

  meta = with lib; {
    description = "Simple Golang SMTP relay/proxy server";
    homepage = https://github.com/decke/smtprelay;
    license = licenses.mit;
  };
}
