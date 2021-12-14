{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "gerrit-queue";
  version = "master";
  vendorSha256 = "0n5h7j416yb2mwic9c3rhqza64jlvl7iw507r9mkw3jadn4whm7a";
  src = ./.;

  meta = with lib; {
    description = "Gerrit submit bot";
    homepage = "https://github.com/tweag/gerrit-queue";
    license = licenses.asl20;
  };
}
