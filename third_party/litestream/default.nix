# TODO(tazjin): Upstream this derivation if litestream is good.
{ pkgs, lib, ... }:

let inherit (pkgs) buildGoModule fetchFromGitHub;
in pkgs.buildGoModule rec {
  pname = "litestream";
  version = "0.3.4";
  vendorSha256 = "1dcb5f06cq484i9m9pi5l4banygh3s4bz5964hy7ypmm1z2pcmrv";

  src = pkgs.fetchFromGitHub {
    owner = "benbjohnson";
    repo = "litestream";
    rev = "v${version}";
    sha256 = "1ivyxgmbysir9qsbsz6nksjqzw6ba0zrymacsw12ab9bvbldjxv7";
  };

  meta = {
    description = "Streaming S3 replication for SQLite";
    homepage = "https://litestream.io/";
    license = lib.licenses.asl20;
    maintainers = [ lib.maintainers.tazjin ];
  };
}
