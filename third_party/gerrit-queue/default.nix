{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "gerrit-queue";
  version = "master";
  vendorSha256 = "0hivr4yn9aa1vk7z1h1nwg75hzqnsaxypi1wwxdy1l1hnm5k8hhi";
  src = ./.;

  # gerrit-queue embeds static assets which need to be generated
  nativeBuildInputs = [ pkgs.statik ];
  preBuild = ''
    statik -f
  '';

  meta = with lib; {
    description = "Gerrit submit bot";
    homepage = "https://github.com/tweag/gerrit-queue";
    license = licenses.asl20;
  };
}
