{ pkgs, ... }:

pkgs.buildGoModule {
  name = "tvl-ebooks";
  vendorSha256 = "1p7bazh2vbhvvm559bcvfff9s4yy4q9jmklxr3sfp97inwpv6hzy";
  src = ./.;
}
