{ pkgs, lib, ... }:

pkgs.buildGoModule {
  pname = "gerrit-queue";
  version = "master";
  vendorSha256 = "1bqllafvd4yy4cy6barpqhycxmhzcx3p5shpzhd8qwxwwg0clxs6";
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
